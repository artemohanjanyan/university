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
import java.util.Arrays;
import java.util.Collections;

/**
 * @author akirakozov
 */
public class QueryServlet extends AbstractProductsServlet {

    public QueryServlet(ProductsDatabase productsDatabase) {
        super(productsDatabase);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String command = request.getParameter("command");

        if ("max".equals(command)) {
            Product maxProduct = getProductsDatabase().getMaxProduct();
            getFormatter().setHeading("Max");
            response.getWriter().println(getFormatter().formatProducts(Collections.singletonList(maxProduct)));
        } else if ("min".equals(command)) {
            Product minProduct = getProductsDatabase().getMinProduct();
            getFormatter().setHeading("Min");
            response.getWriter().println(getFormatter().formatProducts(Collections.singletonList(minProduct)));
        } else if ("sum".equals(command)) {
            long sum = getProductsDatabase().getSum();
            getFormatter().setHeading("Summary price: ");
            response.getWriter().println(getFormatter().formatLong(sum));
        } else if ("count".equals(command)) {
            long count = getProductsDatabase().getSum();
            getFormatter().setHeading("Number of products: ");
            response.getWriter().println(getFormatter().formatLong(count));
        } else {
            getFormatter().setHeading("Unknown command: ");
            response.getWriter().println(getFormatter().formatString(command));
        }

        response.setContentType("text/html");
        response.setStatus(HttpServletResponse.SC_OK);
    }

}
