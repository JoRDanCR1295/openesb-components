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
 * @(#)jspsvgtag.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package org.printx.jspsvg;

import java.io.IOException;
import java.net.URLEncoder;

import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.servlet.jsp.JspWriter;
import javax.servlet.jsp.tagext.BodyContent;
import javax.servlet.jsp.tagext.BodyTagSupport;

public class jspsvgtag extends BodyTagSupport {

private String name="svgservlet";
private String width="0";
private String height="0";

public void setname(String value) {
	this.name = value;
}

public void setwidth(String value) {
	this.width = value;
}

public void setheight(String value) {
	this.height = value;
}



public int doAfterBody() {
	BodyContent body = getBodyContent();

	HttpSession  session 		= pageContext.getSession();
	HttpServletResponse response	= (HttpServletResponse)pageContext.getResponse();
        HttpServletRequest request      =  (HttpServletRequest)pageContext.getRequest();
//        String remoteAddress = request.getScheme() + "://"
//                + request.getServerName() + ":" + request.getServerPort()
//                + request.getContextPath();
 
	// String filteredBody = ServletUtilities.filter(body.getString());
	String contentBody = body.getString();

	// Store svg in session
	session.setAttribute(name,contentBody);

	try {
		JspWriter out = body.getEnclosingWriter();

		// print html vector to svgstream servlet
		// encode also session when possible
		
        String svgServletUrl = 
            response.encodeURL(((HttpServletRequest)pageContext.getRequest()).getContextPath() + 
            "/svgstream.svgz?svgstream="+URLEncoder.encode(name,"UTF-8"));
                //));
        

		out.print("<embed name=\""+name+"\"");
		out.print(" width=\""+width+"\"");
		out.print(" height=\""+height+"\"");
		out.print(" type=\"image/svg-xml\"");
		out.print(" src=\""+svgServletUrl+"\"");
		out.print(" pluginspage=\"http://www.adobe.com/svg/viewer/install/\"");
		out.print(">");

        } 
	catch(IOException ioe) 
	{
		System.out.println("Error in jspservlettag: " + ioe);
	}
	return(SKIP_BODY);
	}
}
