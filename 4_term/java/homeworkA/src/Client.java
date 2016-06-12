import ru.ifmo.ctddev.ohanjanyan.bank.Account;
import ru.ifmo.ctddev.ohanjanyan.bank.Bank;
import ru.ifmo.ctddev.ohanjanyan.bank.Person;

import java.rmi.*;
import java.net.*;
import java.rmi.ConnectException;

public class Client {
    public static void main(String[] args) throws RemoteException {
        Bank bank;
        try {
            bank = (Bank) Naming.lookup("//localhost/bank");
        } catch (Exception e) {
            System.out.println("Bank is offline");
            return;
        }

        String firstName, secondName, passport, accountId;
        int change;

        try {
            firstName = args[0];
            secondName = args[1];
            passport = args[2];
            accountId = args[3];
            change = Integer.parseInt(args[4]);
        } catch (Exception e) {
            System.err.println("Wrong arguments format");
            System.err.println("Usage: java Client <first name> <second name> <passport> <account id> <change>");
            return;
        }

        Person person;
        try {
            person = bank.getRemotePerson(passport);
        } catch (ConnectException e) {
            System.err.println("Server is offline");
            return;
        }

        if (person == null) {
            System.out.println("Creating new person...");
            bank.createPerson(firstName, secondName, passport);
        }

        if (!bank.getAccountIds(passport).contains(accountId)) {
            System.err.println("Creating account...");
            Account account = bank.getAccount(accountId);
            if (account != null) {
                System.err.println("Account already exists for another person!");
                return;
            }
            bank.createAccount(passport, accountId);
        }

        System.out.println("Checking person data...");
        bank.checkPerson(firstName, secondName, passport);

        Account account = bank.getAccount(accountId);
        System.out.println("Current balance: " + account.getAmount());
        System.out.println("Changing balance...");
        account.setAmount(account.getAmount() + change);
        System.out.println("New balance: " + account.getAmount());
    }
}
