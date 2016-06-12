package ru.ifmo.ctddev.ohanjanyan.bank;

import java.io.Serializable;
import java.rmi.RemoteException;

/**
 * Serializable {@link Person}.
 */
public class LocalPerson implements Serializable, Person {

    private final String firstName, secondName, passport;

    LocalPerson(String firstName, String secondName, String passport) {
        this.firstName = firstName;
        this.secondName = secondName;
        this.passport = passport;
    }

    @Override
    public String getFirstName() throws RemoteException {
        return firstName;
    }

    @Override
    public String getSecondName() throws RemoteException {
        return secondName;
    }

    @Override
    public String getPassport() throws RemoteException {
        return passport;
    }
}
