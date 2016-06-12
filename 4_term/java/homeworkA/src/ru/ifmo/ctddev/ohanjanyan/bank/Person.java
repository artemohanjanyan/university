package ru.ifmo.ctddev.ohanjanyan.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

/**
 * Person in bank.
 */
public interface Person extends Remote {

    /**
     * Get first name of a person.
     * @throws RemoteException if communication error occurs.
     */
    String getFirstName() throws RemoteException;

    /**
     * Get second name of a person.
     * @throws RemoteException if communication error occurs.
     */
    String getSecondName() throws RemoteException;

    /**
     * Get passport id of a person.
     * @throws RemoteException if communication error occurs.
     */
    String getPassport() throws RemoteException;
}
