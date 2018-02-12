package ru.akirakozov.sd.refactoring.servlet;

import ru.akirakozov.sd.refactoring.db.Product;
import ru.akirakozov.sd.refactoring.db.ProductsDatabase;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.List;

/**
 * @author akirakozov
 */
public class GetProductsServlet extends AbstractProductsServlet {

    public GetProductsServlet(ProductsDatabase productsDatabase) {
        super(productsDatabase);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        List<Product> products = getProductsDatabase().getProducts();

        response.setContentType("text/html");
        response.setStatus(HttpServletResponse.SC_OK);
        getFormatter().setHeading("All products:");
        response.getWriter().println(getFormatter().formatProducts(products));
    }
}
