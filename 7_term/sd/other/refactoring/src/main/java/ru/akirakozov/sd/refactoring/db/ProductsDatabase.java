package ru.akirakozov.sd.refactoring.db;

import java.util.List;

public interface ProductsDatabase {

    void add(Product product);

    List<Product> getProducts();

    Product getMaxProduct();

    Product getMinProduct();

    int getCount();

    long getSum();
}
