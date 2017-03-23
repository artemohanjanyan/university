package ru.ifmo.pp;

/**
 * Operation on a bank.
 *
 * @author Roman Elizarov
 */
abstract class Operation {
    Object invoke(Bank bank) {
        try {
            return invokeImpl(bank);
        } catch (Throwable t) {
            return t.getClass();
        }
    }

    abstract Object invokeImpl(Bank bank);

    static class GetAmount extends Operation {
        final int index;

        GetAmount(int index) {
            this.index = index;
        }

        @Override
        Object invokeImpl(Bank bank) {
            return bank.getAmount(index);
        }

        @Override
        public String toString() {
            return "GetAmount{" +
                    "index=" + index +
                    '}';
        }
    }

    static class GetTotalAmount extends Operation {
        @Override
        Object invokeImpl(Bank bank) {
            return bank.getTotalAmount();
        }

        @Override
        public String toString() {
            return "GetTotalAmount{}";
        }
    }

    static class Deposit extends Operation {
        final int index;
        final long amount;

        Deposit(int index, long amount) {
            this.index = index;
            this.amount = amount;
        }

        @Override
        Object invokeImpl(Bank bank) {
            return bank.deposit(index, amount);
        }

        @Override
        public String toString() {
            return "Deposit{" +
                    "index=" + index +
                    ", amount=" + amount +
                    '}';
        }
    }

    static class Withdraw extends Operation {
        final int index;
        final long amount;

        Withdraw(int index, long amount) {
            this.index = index;
            this.amount = amount;
        }

        @Override
        Object invokeImpl(Bank bank) {
            return bank.withdraw(index, amount);
        }

        @Override
        public String toString() {
            return "Withdraw{" +
                    "index=" + index +
                    ", amount=" + amount +
                    '}';
        }
    }

    static class Transfer extends Operation {
        final int fromIndex;
        final int toIndex;
        final long amount;

        Transfer(int fromIndex, int toIndex, long amount) {
            this.fromIndex = fromIndex;
            this.toIndex = toIndex;
            this.amount = amount;
        }

        @Override
        Object invokeImpl(Bank bank) {
            bank.transfer(fromIndex, toIndex, amount);
            return null;
        }

        @Override
        public String toString() {
            return "Transfer{" +
                    "fromIndex=" + fromIndex +
                    ", toIndex=" + toIndex +
                    ", amount=" + amount +
                    '}';
        }
    }
}
