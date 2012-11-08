/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)svgservlet.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.printx.jspsvg;

import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;
import java.util.zip.*;

public class svgservlet extends HttpServlet {
public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

//	response.setContentType("image/svg+xml");

	String encodings = request.getHeader("Accept-Encoding");
	String encodeFlag = request.getParameter("encoding");


	HttpSession session = request.getSession(true);

	String svgstream = request.getParameter("svgstream");
	String contentSVG = (String)session.getAttribute(svgstream);

	PrintWriter out;
	String title;

	if ((encodings != null) && (encodings.indexOf("gzip") != -1) && !"none".equals(encodeFlag)) {
		title = "Page Encoded with GZip";
		OutputStream out1 = response.getOutputStream();
		// OutputStream out1 =bodyOut.getEnclosingWriter();
		out = new PrintWriter(new GZIPOutputStream(out1), false);
		response.setHeader("Content-Encoding", "gzip");
		
	} else {
		title = "Unencoded Page";
		out = response.getWriter();
	}

	out.println(contentSVG.trim());
	out.close();
}
}
