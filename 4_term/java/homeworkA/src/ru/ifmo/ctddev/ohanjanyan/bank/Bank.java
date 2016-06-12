package ru.ifmo.ctddev.ohanjanyan.bank;

import java.rmi.*;
import java.util.Set;

/**
 * Bank model interface.
 */
public interface Bank extends Remote {

    /**
     * @param passport id of a person.
     * @return corresponding local person, if exists, null otherwise.
     * @throws RemoteException if communication error occurs.
     */
    LocalPerson getLocalPerson(String passport) throws RemoteException;

    /**
     * @param passport id of a person.
     * @return corresponding remote person, if exists, null otherwise.
     * @throws RemoteException if communication error occurs.
     */
    Person getRemotePerson(String passport) throws RemoteException;

    /**
     * Get account ids registered for a person.
     * @param passport id of the person.
     * @return set of accounts registered for the person, if person exists, null otherwise.
     * @throws RemoteException if communication error occurs.
     */
    Set<String> getAccountIds(String passport) throws RemoteException;

    /**
     * Register new person in bank with given first name, second name and passport id.
     * @return true on success, false if person with same passport is already registered.
     * @throws RemoteException if communication error occurs.
     */
    boolean createPerson(String firstName, String secondName, String passport) throws RemoteException;

    /**
     * Check if person with given first name, second name and passport id registered in the bank.
     * @throws RemoteException if communication error occurs.
     */
    boolean checkPerson(String firstName, String secondName, String passport) throws RemoteException;

    /**
     * Create new empty account and associate it with a person.
     * @param person passport id of a person.
     * @param id new account id.
     * @return created account, or null if account with the same id already exists.
     * @throws IllegalArgumentException if no person with given passport is registered.
     * @throws RemoteException if communication error occurs.
     */
    Account createAccount(String person, String id) throws RemoteException;

    /**
     * @param id account id.
     * @return account if it exists in the bank, null otherwise.
     * @throws RemoteException if communication error occurs.
     */
    Account getAccount(String id) throws RemoteException;
}
