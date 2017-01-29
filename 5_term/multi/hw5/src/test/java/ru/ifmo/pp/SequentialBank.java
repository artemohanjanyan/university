package ru.ifmo.pp;

import ru.ifmo.pp.Bank;

/**
 * Bank implementation.
 * This implementation is not thread-safe.
 *
 * @author Roman Elizarov
 */
class SequentialBank implements Bank {
    /**
     * An array of accounts by index.
     */
    private final Account[] accounts;

    /**
     * Creates new bank instance.
     * @param n the number of accounts (numbered from 0 to n-1).
     */
    public SequentialBank(int n) {
        accounts = new Account[n];
        for (int i = 0; i < n; i++) {
            accounts[i] = new Account();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int getNumberOfAccounts() {
        return accounts.length;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getAmount(int index) {
        return accounts[index].amount;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long getTotalAmount() {
        long sum = 0;
        for (Account account : accounts) {
            sum += account.amount;
        }
        return sum;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long deposit(int index, long amount) {
        if (amount <= 0)
            throw new IllegalArgumentException("Invalid amount: " + amount);
        Account account = accounts[index];
        if (amount > MAX_AMOUNT || account.amount + amount > MAX_AMOUNT)
            throw new IllegalStateException("Overflow");
        account.amount += amount;
        return account.amount;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long withdraw(int index, long amount) {
        if (amount <= 0)
            throw new IllegalArgumentException("Invalid amount: " + amount);
        Account account = accounts[index];
        if (account.amount - amount < 0)
            throw new IllegalStateException("Underflow");
        account.amount -= amount;
        return account.amount;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void transfer(int fromIndex, int toIndex, long amount) {
        if (amount <= 0)
            throw new IllegalArgumentException("Invalid amount: " + amount);
        if (fromIndex == toIndex)
            throw new IllegalArgumentException("fromIndex == toIndex");
        Account from = accounts[fromIndex];
        Account to = accounts[toIndex];
        if (amount > from.amount)
            throw new IllegalStateException("Underflow");
        else if (amount > MAX_AMOUNT || to.amount + amount > MAX_AMOUNT)
            throw new IllegalStateException("Overflow");
        from.amount -= amount;
        to.amount += amount;
    }

    /**
     * Private account data structure.
     */
    private static class Account {
        /**
         * Amount of funds in this account.
         */
        long amount;
    }
}
