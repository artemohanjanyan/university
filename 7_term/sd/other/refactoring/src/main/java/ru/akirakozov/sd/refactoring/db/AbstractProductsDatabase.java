package ru.akirakozov.sd.refactoring.db;

import java.util.Comparator;

public abstract class AbstractProductsDatabase implements ProductsDatabase {
    @Override
    public Product getMaxProduct() {
        return getProducts().stream()
                .max(Comparator.comparingLong(Product::getPrice))
                .orElse(null);
    }

    @Override
    public Product getMinProduct() {
        return getProducts().stream()
                .min(Comparator.comparingLong(Product::getPrice))
                .orElse(null);
    }

    @Override
    public int getCount() {
        return getProducts().size();
    }

    @Override
    public long getSum() {
        return getProducts().stream()
                .mapToLong(Product::getPrice)
                .sum();
    }
}
