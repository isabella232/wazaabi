package org.eclipse.wazaabi.debug.ui;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class SimpleServlet extends HttpServlet {

	private static final long serialVersionUID = 1L;

	protected void doGet(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		resp.setContentType("text/plain");
		resp.getWriter().write("Hello from the cloud!");
	}

	protected void doPost(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		byte[] buffer = new byte[1024];
		int length = 0;
		while ((length = req.getInputStream().read(buffer)) != -1)
			baos.write(buffer, 0, length);

		if (Activator.getModelDisplay() != null)
			Activator.getModelDisplay().processCommand(
					new String(baos.toByteArray()));
		// Jetty seems to need to return something
		resp.setContentType("text/plain");
		resp.getWriter().write("");

	}
}