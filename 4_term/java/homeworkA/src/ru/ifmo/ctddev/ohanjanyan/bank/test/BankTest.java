package ru.ifmo.ctddev.ohanjanyan.bank.test;

import ru.ifmo.ctddev.ohanjanyan.bank.*;

import java.rmi.Naming;
import java.rmi.RemoteException;
import java.util.*;

import static org.junit.Assert.*;

/**
 * Tests bank model.
 */
public class BankTest {

    private static Bank bank;

    private static final int PEOPLE = 100;
    private static final int ACCOUNTS = 1000;

    /**
     * Launches bank and inflates it with some people and accounts.
     * @throws Exception if failed to launch and inflate bank.
     */
    @org.junit.BeforeClass
    public static void beforeClass() throws Exception {
        Naming.rebind("//localhost/bank", new BankImpl(8888));
        bank = (Bank) Naming.lookup("//localhost/bank");
        Random random = new Random(2539);

        for (int i = 0; i < PEOPLE; ++i) {
            assertTrue(bank.createPerson("John_" + i, "Banker", "" + i));
            bank.createAccount("" + i, "" + (-i - 1));
        }

        for (int i = 0; i < ACCOUNTS; ++i) {
            Account account = bank.createAccount("" + random.nextInt(PEOPLE), "" + i);
            account.setAmount(random.nextInt());
        }

        System.out.println("Bank inflated");
    }

    /**
     * Test {@link Bank#getLocalPerson(String)} and {@link Bank#getRemotePerson(String)} methods.
     * @throws RemoteException if communication error occurs.
     */
    @org.junit.Test
    public void getPerson() throws RemoteException {
        assertNull(bank.getLocalPerson("" + (-PEOPLE - 1)));
        assertNull(bank.getRemotePerson("" + (-PEOPLE - 1)));
        for (int i = 0; i < PEOPLE; ++i) {
            Person person = bank.getLocalPerson("" + i);
            assertNotNull(person);
            Person remotePerson = bank.getRemotePerson(person.getPassport());
            assertEquals(person.getFirstName(), remotePerson.getFirstName());
            assertEquals(person.getSecondName(), remotePerson.getSecondName());
            assertEquals(person.getPassport(), remotePerson.getPassport());
        }
    }

    /**
     * Test {@link Bank#getAccountIds(String)}.
     * @throws RemoteException if communication error occurs.
     */
    @org.junit.Test
    public void getAccountIds() throws RemoteException {
        for (int i = 0; i < PEOPLE; ++i) {
            Set<String> ids = bank.getAccountIds("" + i);
            assertNotNull(ids);
            assertTrue(ids.size() > 0);
        }
    }

    /**
     * Test {@link Bank#getAccountIds(String)}.
     * @throws RemoteException if communication error occurs.
     */
    @org.junit.Test
    public void createPerson() throws RemoteException {
        for (int i = 0; i < PEOPLE; ++i) {
            assertFalse(bank.createPerson("James_" + i, "Programmer", "" + i));
            assertTrue(bank.createPerson("James_" + i, "Programmer", "" + (PEOPLE + i)));
        }
    }

    /**
     * Test {@link Bank#checkPerson(String, String, String)}.
     * @throws RemoteException if communication error occurs.
     */
    @org.junit.Test
    public void checkPerson() throws RemoteException {
        for (int i = 0; i < PEOPLE; ++i) {
            assertTrue(bank.checkPerson("John_" + i, "Banker", "" + i));
            assertFalse(bank.checkPerson("James", "Programmer", "" + i));
        }
    }

    /**
     * Test {@link Bank#createAccount(String, String)}.
     * @throws RemoteException if communication error occurs.
     */
    @org.junit.Test
    public void createAccount() throws RemoteException {
        try {
            bank.createAccount("-1", "0");
            fail("Account with id 0 already exists, exception should be thrown");
        } catch (IllegalArgumentException | RemoteException ignored) {
        } catch (Exception e) {
            fail("IllegalArgumentException exception expected");
        }

        for (int i = 0; i < ACCOUNTS; ++i) {
            assertNull(bank.createAccount("0", "" + i));
        }

        Account account = bank.createAccount("0", "" + (-PEOPLE - PEOPLE));
        assertNotNull(account);
        assertEquals(account.getAmount(), 0);
    }

    /**
     * Test {@link Bank#getAccount(String)}.
     * @throws RemoteException if communication error occurs.
     */
    @org.junit.Test
    public void getAccount() throws RemoteException {
        for (int i = 0; i < ACCOUNTS; ++i) {
            Account account = bank.getAccount("" + i);
            assertNotNull(account);
            account.setAmount(-1);
            assertEquals(-1, account.getAmount());
        }
        assertNull(bank.getAccount("" + (-PEOPLE - PEOPLE - 1)));
    }

}