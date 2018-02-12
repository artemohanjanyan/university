package ru.akirakozov.sd.refactoring.servlet;

import ru.akirakozov.sd.refactoring.db.Product;
import ru.akirakozov.sd.refactoring.db.ProductsDatabase;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * @author akirakozov
 */
public class AddProductServlet extends AbstractProductsServlet {

    public AddProductServlet(ProductsDatabase productsDatabase) {
        super(productsDatabase);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String name = request.getParameter("name");
        long price = Long.parseLong(request.getParameter("price"));

        getProductsDatabase().add(new Product(name, price));

        response.setContentType("text/html");
        response.setStatus(HttpServletResponse.SC_OK);
        getFormatter().setHeading(null);
        response.getWriter().println(getFormatter().formatString("OK"));
    }
}
