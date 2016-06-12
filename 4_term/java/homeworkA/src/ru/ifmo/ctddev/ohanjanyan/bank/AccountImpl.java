package ru.ifmo.ctddev.ohanjanyan.bank;

import java.rmi.*;
import java.rmi.server.UnicastRemoteObject;

class AccountImpl extends UnicastRemoteObject implements Account {
    private final String id;
    private int amount;

    AccountImpl(int port, String id) throws RemoteException {
        super(port);
        this.id = id;
        amount = 0;
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public int getAmount() {
        return amount;
    }

    @Override
    public void setAmount(int amount) {
        this.amount = amount;
    }
}
