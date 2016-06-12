package ru.ifmo.ctddev.ohanjanyan.bank;

import java.rmi.*;

/**
 * Account in bank.
 */
public interface Account extends Remote {

    /**
     * Get id of account.
     * @throws RemoteException if communication error occurs.
     */
    String getId() throws RemoteException;

    /**
     * Get amount of money on account.
     * @throws RemoteException if communication error occurs.
     */
    int getAmount() throws RemoteException;

    /**
     * Set amount of money on account.
     * @throws RemoteException if communication error occurs.
     */
    void setAmount(int amount) throws RemoteException;
}
