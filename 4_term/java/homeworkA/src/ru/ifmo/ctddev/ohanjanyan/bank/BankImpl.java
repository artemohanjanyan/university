package ru.ifmo.ctddev.ohanjanyan.bank;

import java.util.*;
import java.rmi.server.*;
import java.rmi.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentSkipListSet;

/**
 * Implementation of bank model.
 */
public class BankImpl extends UnicastRemoteObject implements Bank {

    private final ConcurrentMap<String, Person> people = new ConcurrentHashMap<>();
    private final ConcurrentMap<String, Account> accounts = new ConcurrentHashMap<>();
    private final ConcurrentMap<String, Set<String>> passportAccounts = new ConcurrentHashMap<>();
    private final int port;

    public BankImpl(final int port) throws RemoteException {
        super(port);
        this.port = port;
    }

    @Override
    public LocalPerson getLocalPerson(String passport) throws RemoteException {
        Person person = people.get(passport);
        if (person == null) {
            return null;
        }
        return new LocalPerson(person.getFirstName(), person.getSecondName(), person.getPassport());
    }

    @Override
    public Person getRemotePerson(String passport) throws RemoteException {
        Person person = people.get(passport);
        if (person == null) {
            return null;
        }
        return new RemotePerson(person.getFirstName(), person.getSecondName(), person.getPassport());
    }

    @Override
    public Set<String> getAccountIds(String passport) throws RemoteException {
        return passportAccounts.get(passport);
    }

    @Override
    public boolean createPerson(String firstName, String secondName, String passport) throws RemoteException {
        if (people.get(passport) != null) {
            return false;
        }

        people.put(passport, new LocalPerson(firstName, secondName, passport));
        passportAccounts.put(passport, new ConcurrentSkipListSet<>());
        return true;
    }

    @Override
    public boolean checkPerson(String firstName, String secondName, String passport) throws RemoteException {
        Person person = people.get(passport);
        return person != null &&
                person.getFirstName().equals(firstName) && person.getSecondName().equals(secondName);
    }

    @Override
    public Account createAccount(String passport, String id) throws RemoteException {
        if (!people.containsKey(passport)) {
            throw new IllegalArgumentException("no person associated with passport");
        }
        if (accounts.containsKey(id)) {
            return null;
        }

        Account account = new AccountImpl(port, id);
        accounts.put(id, account);

        passportAccounts.get(passport).add(account.getId());

        return account;
    }

    @Override
    public Account getAccount(String id) {
        return accounts.get(id);
    }
}
