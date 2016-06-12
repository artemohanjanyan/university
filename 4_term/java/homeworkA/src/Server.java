import ru.ifmo.ctddev.ohanjanyan.bank.Bank;
import ru.ifmo.ctddev.ohanjanyan.bank.BankImpl;

import java.rmi.*;

public class Server {
    private final static int PORT = 8888;
    public static void main(String[] args) {
        try {
            Bank bank = new BankImpl(PORT);
            Naming.rebind("//localhost/bank", bank);
        } catch (Exception e) {
            System.out.println("Can't start server");
            return;
        }
        System.out.println("Server started");
    }
}
