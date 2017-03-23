package ru.ifmo.pp;

import java.util.concurrent.atomic.AtomicReferenceArray;

/**
 * Bank implementation.
 * This class is thread-safe and lock-free using operation objects.
 *
 * <p>This implementation is based on "A Practical Multi-Word Compare-and-Swap Operation"  by T. L. Harris et al.
 * It uses a simplified and faster version of DCSS operation that relies for its correctness on the fact that
 * Account instances in {@link #accounts} array never suffer from ABA problem.
 * See also "Practical lock-freedom" by Keir Fraser.
 * See {@link #acquire(int, Op)} method.
 *
 * @author Оганджанян
 */
public class BankImpl implements Bank {
    /**
     * An array of accounts by index.
     * Account instances here are never reused (there is no ABA).
     */
    private final AtomicReferenceArray<Account> accounts;

    /**
     * Creates new bank instance.
     * @param n the number of accounts (numbered from 0 to n-1).
     */
    public BankImpl(int n) {
        accounts = new AtomicReferenceArray<>(n);
        for (int i = 0; i < n; i++) {
            accounts.set(i, new Account(0));
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getNumberOfAccounts() {
        return accounts.length();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getAmount(int index) {
        while (true) {
            Account account = accounts.get(index);
            /*
             * If there is a pending operation on this account, then help to complete it first using
             * its invokeOperation method. If the result is false then there is no pending operation,
             * thus the account amount can be safely returned.
             */
            if (!account.invokeOperation())
                return account.amount;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getTotalAmount() {
        /*
         * This operation requires atomic read of all accounts, thus it creates an operation descriptor.
         * Operation's invokeOperation method acquires all accounts, computes the total amount, and releases
         * all accounts. This method returns the result.
         */
        TotalAmountOp op = new TotalAmountOp();
        op.invokeOperation();
        return op.sum;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long deposit(int index, long amount) {
        // First, validate method per-conditions
        if (amount <= 0)
            throw new IllegalArgumentException("Invalid amount: " + amount);
        if (amount > MAX_AMOUNT)
            throw new IllegalStateException("Overflow");
        /*
         * This operation depends only on a single account, thus it can be directly
         * performed using a regular lock-free compareAndSet loop.
         */
        while (true) {
            Account account = accounts.get(index);
            /*
             * If there is a pending operation on this account, then help to complete it first using
             * its invokeOperation method. If the result is false then there is no pending operation,
             * thus the account can be safely updated.
             */
            if (!account.invokeOperation()) {
                if (account.amount + amount > MAX_AMOUNT)
                    throw new IllegalStateException("Overflow");
                Account updated = new Account(account.amount + amount);
                if (accounts.compareAndSet(index, account, updated))
                    return updated.amount;
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long withdraw(int index, long amount) {
        if (amount <= 0)
            throw new IllegalArgumentException("Invalid amount: " + amount);

        while (true) {
            Account account = accounts.get(index);
            if (!account.invokeOperation()) {
                if (account.amount - amount < 0)
                    throw new IllegalStateException("Underflow");
                Account updated = new Account(account.amount - amount);
                if (accounts.compareAndSet(index, account, updated))
                    return updated.amount;
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void transfer(int fromIndex, int toIndex, long amount) {
        // First, validate method per-conditions
        if (amount <= 0)
            throw new IllegalArgumentException("Invalid amount: " + amount);
        if (fromIndex == toIndex)
            throw new IllegalArgumentException("fromIndex == toIndex");
        if (amount > MAX_AMOUNT)
            throw new IllegalStateException("Underflow/overflow");
        /*
         * This operation requires atomic read of two accounts, thus it creates an operation descriptor.
         * Operation's invokeOperation method acquires both accounts, computes the result of operation
         * (if a form of error message), and releases both accounts. This method throws the exception with
         * the corresponding message if needed.
         */
        TransferOp op = new TransferOp(fromIndex, toIndex, amount);
        op.invokeOperation();
        if (op.errorMessage != null)
            throw new IllegalStateException(op.errorMessage);
    }

    /**
     * This is an implementation of a restricted form of Harris DCSS operation:
     * It atomically checks that op.completed is false and replaces accounts[index] with AcquiredAccount instance
     * that hold a reference to the op.
     * This method returns null if op.completed is true.
     */
    private AcquiredAccount acquire(int index, Op op) {
        /*
         * This method must loop trying to replace accounts[index] with an instance of
         *     new AcquiredAccount(<old-amount>, op) until that successfully happens and return the
         *     instance of AcquiredAccount in this case.
         *
         * If current account is already "Acquired" by another operation, then this method must help that
         * other operation by invoking "invokeOperation" and continue trying.
         *
         * Because accounts[index] does not have an ABA problem, there is no need to implement full-blown
         * DCSS operation with descriptors for DCSS operation as explained in Harris CASN work. A simple
         * lock-free compareAndSet loop suffices here if op.completed is checked after the accounts[index]
         * is read.
         */

        while (true) {
            Account account = accounts.get(index);
            if (op.completed) {
                return null;
            }

            if (account instanceof AcquiredAccount) {
                AcquiredAccount acquiredAccount = (AcquiredAccount) account;
                if ((acquiredAccount).op == op) {
                    return acquiredAccount;
                }
                acquiredAccount.invokeOperation();
            } else {
                AcquiredAccount acquiredAccount = new AcquiredAccount(account.amount, op);
                if (accounts.compareAndSet(index, account, acquiredAccount)) {
                    return acquiredAccount;
                }
            }
        }
    }

    /**
     * Releases an account that was previously acquired by {@link #acquire(int, Op)}.
     * This method does nothing if the account at index is not currently acquired.
     */
    private void release(int index, Op op) {
        assert op.completed; // must be called only on operations that were already completed
        Account account = accounts.get(index);
        if (account instanceof AcquiredAccount) {
            AcquiredAccount acquiredAccount = (AcquiredAccount) account;
            if (acquiredAccount.op == op) {
                // release performs update at most once while the account is still acquired
                Account updated = new Account(acquiredAccount.newAmount);
                accounts.compareAndSet(index, account, updated);
            }
        }
    }

    /**
     * Immutable account data structure.
     */
    private static class Account {
        /**
         * Amount of funds in this account.
         */
        final long amount;

        Account(long amount) {
            this.amount = amount;
        }

        /**
         * Invokes operation that is pending on this account.
         * This implementation returns false (no pending operation),
         * other implementations return true.
         */
        boolean invokeOperation() {
            return false;
        }
    }

    /**
     * Account that was acquired as a part of in-progress operation that spans multiple accounts.
     * @see #acquire(int, Op)
     */
    private static class AcquiredAccount extends Account {
        final Op op;

        /**
         * New amount of funds in this account when op completes.
         */
        long newAmount;

        AcquiredAccount(long amount, Op op) {
            super(amount);
            this.op = op;
            this.newAmount = amount;
        }

        @Override
        boolean invokeOperation() {
            op.invokeOperation();
            return true;
        }
    }

    /**
     * Abstract operation that acts on multiple accounts.
     */
    private abstract class Op {
        /**
         * True when operation has completed.
         */
        volatile boolean completed;

        abstract void invokeOperation();
    }

    /**
     * Descriptor for {@link #getTotalAmount()} operation.
     */
    private class TotalAmountOp extends Op {
        /**
         * The result of getTotalAmount operation is stored here before setting
         * {@link #completed} to true.
         */
        long sum;

        @Override
        void invokeOperation() {
            long sum = 0;
            int i;
            int n = accounts.length();
            for (i = 0; i < n; i++) {
                AcquiredAccount account = acquire(i, this);
                if (account == null)
                    break;
                sum += account.amount;
            }
            if (i == n) {
                /*
                 * If i == n, then all acquired accounts were not null and full sum was calculated.
                 * this.sum = sum assignment below has a benign data race. Multiple threads might to this assignment
                 * concurrently, however, they are all guaranteed to be assigning the same value.
                 */
                this.sum = sum;
                this.completed = true; // volatile write to completed field _after_ the sum was written
            }
            /*
             * As performance optimization, only acquired accounts are released. There is no harm in calling
             * release for all accounts, though.
             */
            for (; --i >= 0;) {
                release(i, this);
            }
        }
    }

    /**
     * Descriptor for {@link #transfer(int, int, long) transfer(...)} operation.
     */
    private class TransferOp extends Op {
        final int fromIndex;
        final int toIndex;
        final long amount;

        String errorMessage;

        TransferOp(int fromIndex, int toIndex, long amount) {
            this.fromIndex = fromIndex;
            this.toIndex = toIndex;
            this.amount = amount;
        }

        @Override
        void invokeOperation() {
            /*
             * In the implementation of this operation only two accounts (with fromIndex and toIndex) needs
             * to be acquired. Unlike TotalAmountOp, this operation has its own result in errorMessage string
             * and it must also update AcquiredAccount.newAmount fields before setting completed to true
             * and invoking release on those acquired accounts.
             *
             * Basically, implementation of this method must perform the logic of the following code "atomically":
             */

            AcquiredAccount from, to;
            if (fromIndex < toIndex) {
                from = acquire(fromIndex, this);
                to = acquire(toIndex, this);
            } else {
                to = acquire(toIndex, this);
                from = acquire(fromIndex, this);
            }

            if (from != null && to != null) {
                if (amount > from.amount)
                    errorMessage = "Underflow";
                else if (to.amount + amount > MAX_AMOUNT)
                    errorMessage = "Overflow";
                else {
                    from.newAmount = from.amount - amount;
                    to.newAmount = to.amount + amount;
                }
                this.completed = true;
            }

            if (fromIndex < toIndex) {
                release(toIndex, this);
                release(fromIndex, this);
            } else {
                release(fromIndex, this);
                release(toIndex, this);
            }
        }
    }
}
