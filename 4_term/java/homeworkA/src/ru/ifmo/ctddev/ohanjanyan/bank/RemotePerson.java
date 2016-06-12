package ru.ifmo.ctddev.ohanjanyan.bank;

import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

class RemotePerson extends UnicastRemoteObject implements Person {

    private final String firstName, secondName, passport;

    RemotePerson(String firstName, String secondName, String passport) throws RemoteException {
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
