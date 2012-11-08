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
 * @(#)BCCoyoteRequest.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc.embedded;

import com.sun.jbi.internationalization.Messages;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.logging.Logger;
import java.util.logging.Level;

import javax.servlet.ServletRequest;
import javax.servlet.http.Cookie;

import org.apache.catalina.Context;
import org.apache.catalina.Globals;
import org.apache.coyote.tomcat5.CoyoteRequest;
import org.apache.coyote.tomcat5.CoyoteResponse;
import org.apache.coyote.tomcat5.CoyoteConnector;
import org.apache.coyote.tomcat5.CoyoteInputStream;
import org.apache.coyote.tomcat5.CoyoteReader;
import org.apache.coyote.tomcat5.InputBuffer;
import org.apache.tomcat.util.http.Parameters;
import com.sun.enterprise.web.PwcWebModule;
import com.sun.enterprise.web.session.SessionCookieConfig;

/**
 * Customized version of the Tomcat 5 CoyoteRequest
 * This is required for supporting Web Programmatic Login and setting the
 * request encoding (charset).
 *
 */
public class BCCoyoteRequest extends CoyoteRequest {

    private static final Messages mMessages =
        Messages.getMessages(BCCoyoteRequest.class);

    //private static Logger logger = LogDomains.getLogger(LogDomains.PWC_LOGGER);
    private final static Logger logger =
        Messages.getLogger(BCCoyoteRequest.class);

    private boolean requestEncodingSet = false;

    // START SJSAS 6346738
    private byte[] formData = null;
    private int formDataLen = 0;
    // END SJSAS 6346738

    public void setContext(Context ctx) {
        super.setContext(ctx);
        CoyoteResponse response = (CoyoteResponse) getResponse();
        // Assert response!=null
        if (response != null) {
            String[] cacheControls = ((PwcWebModule) ctx).getCacheControls();
            for (int i=0; cacheControls!=null && i<cacheControls.length; i++) {
                response.addHeader("Cache-Control", cacheControls[i]);
            }
        }

        requestEncodingSet = false;
    }

    public BufferedReader getReader() throws IOException {
        setRequestEncoding();
        return super.getReader();
    }


    /**
     * Return the character encoding for this Request.
     *
     * If there is no request charset specified in the request, determines and
     * sets the request charset using the locale-charset-info,
     * locale-charset-map, and parameter-encoding elements provided in the
     * sun-web.xml.
     */
    public String getCharacterEncoding() {
        String enc = super.getCharacterEncoding();
        if (enc != null) {
            return enc;
        }
    
        setRequestEncoding();
        return super.getCharacterEncoding();
    }


    /*
     * Configures the given JSESSIONID cookie with the cookie-properties from
     * sun-web.xml.
     *
     * @param cookie The JSESSIONID cookie to be configured
     */
    public void configureSessionCookie(Cookie cookie) {

        super.configureSessionCookie(cookie);

        PwcWebModule wm = (PwcWebModule) getContext();
        SessionCookieConfig cookieConfig = wm.getSessionCookieConfig();

        if (cookieConfig != null) {

            String name = cookieConfig.getName();
            if (name != null && !name.equals(Globals.SESSION_COOKIE_NAME)) {
                logger.log(Level.WARNING,
                           "HTTPBC-W00619.Illegal_cookie_name",
                           new String[] { name, Globals.SESSION_COOKIE_NAME });
            }
     
            if (cookieConfig.getPath() != null) {
                cookie.setPath(cookieConfig.getPath());
            }

            cookie.setMaxAge(cookieConfig.getMaxAge());

            if (cookieConfig.getDomain() != null) {
                cookie.setDomain(cookieConfig.getDomain());
            }

            if (cookieConfig.getComment() != null) {
                cookie.setVersion(1);
                cookie.setComment(cookieConfig.getComment());
            }
        }
    }
    

    // START SJSAS 6346738
    public void recycle() {
        super.recycle();
        formDataLen = 0;
    }
    // END SJSAS 6346738
            

    /**
     * If there is no request charset specified in the request, determines and
     * sets the request charset using the locale-charset-info,
     * locale-charset-map, and parameter-encoding elements provided in the
     * sun-web.xml.
     */
    private void setRequestEncoding() {
 
        if (requestEncodingSet) {
            return;
        }

        requestEncodingSet = true;

        if (super.getCharacterEncoding() != null) {
            return;
        }

        PwcWebModule wm = (PwcWebModule) getContext();

        String encoding = getFormHintFieldEncoding(wm);
        if (encoding == null) {
            encoding = wm.getDefaultCharset();
            if (encoding == null && wm.hasLocaleToCharsetMapping()) {
                encoding = wm.mapLocalesToCharset(getLocales());
            }
        }

        if (encoding != null) {
            try {
                setCharacterEncoding(encoding);
            } catch (UnsupportedEncodingException uee) {
                if (logger.isLoggable(Level.WARNING)) {
                    String text = mMessages.getString("HTTPBC-W00611.Unsupported_character_encoding", new Object[] { encoding, uee.getLocalizedMessage() } );
                    logger.log(Level.WARNING, "text", uee);
                }
            }
        }
    }


    /*
     * Returns the value of the query parameter whose name
     * corresponds to the value of the form-hint-field attribute of the
     * <parameter-encoding> element in sun-web.xml.
     *
     * @return The value of the query parameter whose name corresponds to the
     * value of the form-hint-field attribute in sun-web.xml, or null if the
     * request does not have any query string, or the query string does not
     * contain a query parameter with that name
     */
    private String getFormHintFieldEncoding(PwcWebModule wm) {

        String encoding = null;

        String formHintField = wm.getFormHintField();
        if (formHintField == null){
            return null;
        }

        if ("POST".equalsIgnoreCase(getMethod())) {
            // POST
            encoding = getPostDataEncoding(formHintField);
        } else {
            String query = getQueryString();
            if (query != null) {
                encoding = parseFormHintField(query, formHintField);
            }
        }

        return encoding;
    }
        
    
    private String getPostDataEncoding(String formHintField) {

        if (!getMethod().equalsIgnoreCase("POST")) {
            return null;
        }

        String contentType = getContentType();
        if (contentType == null)
            contentType = "";
        int semicolon = contentType.indexOf(';');
        if (semicolon >= 0) {
            contentType = contentType.substring(0, semicolon).trim();
        } else {
            contentType = contentType.trim();
        }
        if (!("application/x-www-form-urlencoded".equals(contentType))) {
            return null;
        }

        int len = getContentLength();
        if (len <= 0) {
            return null;
        }
        int maxPostSize = ((CoyoteConnector) connector).getMaxPostSize();
        if ((maxPostSize > 0) && (len > maxPostSize)) {
            if (logger.isLoggable(Level.WARNING)) {
                logger.log(Level.WARNING, "HTTPBC-W00610.Post_size_too_large",
                        new Object[] { Integer.valueOf(len), Integer.valueOf(maxPostSize) });
            }
            throw new IllegalStateException("Post too large");
        }

        String encoding = null;

        try {
            formData = null;
            if (len < CACHED_POST_LEN) {
                if (postData == null)
                    postData = new byte[CACHED_POST_LEN];
                formData = postData;
            } else {
                formData = new byte[len];
            }
            int actualLen = readPostBody(formData, len);
            if (actualLen == len) {
                // START SJSAS 6346738
                formDataLen = actualLen;
                // END SJSAS 6346738
                String formDataString = new String(formData).substring(0, len);
                encoding = parseFormHintField(formDataString, formHintField);
            }
        } catch (Throwable t) {
            ; // Ignore
        }

        return encoding;
    }


    /*
     * Parses the value of the specified form-hint-field from the given
     * parameter string.
     *
     * @param paramsString Parameter string
     * @param formHintField From-hint-field
     *
     * @return Value of form-hint-field, or null if not found
     */
    private String parseFormHintField(String paramsString,
                                      String formHintField) {

        String encoding = null;

        formHintField += "=";            
        int index = paramsString.indexOf(formHintField);
        if (index != -1) {
            int endIndex = paramsString.indexOf('&', index);
            if (endIndex != -1) {
                encoding = paramsString.substring(
                    index + formHintField.length(), endIndex);
            } else {
                encoding = paramsString.substring(
                    index + formHintField.length());
            }
        }

        return encoding;
    }


    // START SJSAS 6346738
    /**
     * Gets the POST body of this request.
     *
     * @return The POST body of this request
     */
    protected byte[] getPostBody() throws IOException {

        if (formDataLen > 0) {
            // POST body already read
            return formData;
        } else {
            return super.getPostBody();
        } 
    }
    // END SJSAS 6346738
}
