package ru.akirakozov.sd.refactoring.db;

import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class SQLiteProductsDatabase implements ProductsDatabase {

    private String url;

    public SQLiteProductsDatabase(String url) throws SQLException {
        this.url = url;
        createTable();
    }

    @FunctionalInterface
    private interface ResultConsumer {
        void consume(ResultSet result) throws Exception;
    }

    private static class Wrapper<T> {
        T value;
    }

    private void runQueryWithResult(String sql, ResultConsumer resultConsumer) {
        try (Connection c = DriverManager.getConnection(url);
             Statement stmt = c.createStatement()) {
            ResultSet resultSet = stmt.executeQuery(sql);
            if (resultConsumer != null) {
                while (resultSet.next()) {
                    resultConsumer.consume(resultSet);
                }
            }
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private void runQuery(String sql) {
        try (Connection c = DriverManager.getConnection(url);
             Statement stmt = c.createStatement()) {
            stmt.execute(sql);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private void createTable() throws SQLException {
        String sql = "CREATE TABLE IF NOT EXISTS PRODUCT" +
                "(ID INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL," +
                " NAME           TEXT    NOT NULL, " +
                " PRICE          INT     NOT NULL)";
        runQuery(sql);
    }

    @Override
    public void add(Product product) {
        String sql = "INSERT INTO PRODUCT " +
                "(NAME, PRICE) VALUES (\"" + product.getName() + "\"," + product.getPrice() + ")";
        runQuery(sql);
    }

    @Override
    public List<Product> getProducts() {
        String sql = "SELECT * FROM PRODUCT";
        List<Product> products = new ArrayList<>();

        runQueryWithResult(sql, result -> {
            String name = result.getString("name");
            int price = result.getInt("price");
            products.add(new Product(name, price));
        });

        return products;
    }

    private Product getProduct(String sql) {
        Wrapper<Product> productWrapper = new Wrapper<>();
        runQueryWithResult(sql, result -> {
            String  name = result.getString("name");
            long price  = result.getLong("price");
            productWrapper.value = new Product(name, price);
        });
        return productWrapper.value;
    }

    @Override
    public Product getMaxProduct() {
        return getProduct("SELECT * FROM PRODUCT ORDER BY PRICE DESC LIMIT 1");
    }

    @Override
    public Product getMinProduct() {
        return getProduct("SELECT * FROM PRODUCT ORDER BY PRICE LIMIT 1");
    }

    @Override
    public int getCount() {
        String sql = "SELECT COUNT(price) FROM PRODUCT";
        Wrapper<Integer> countWrapper = new Wrapper<>();
        runQueryWithResult(sql, result -> countWrapper.value = result.getInt(1));
        return countWrapper.value;
    }

    @Override
    public long getSum() {
        String sql = "SELECT SUM(*) FROM PRODUCT";
        Wrapper<Long> sumWrapper = new Wrapper<>();
        runQueryWithResult(sql, result -> sumWrapper.value = result.getLong(1));
        return sumWrapper.value;
    }
}
