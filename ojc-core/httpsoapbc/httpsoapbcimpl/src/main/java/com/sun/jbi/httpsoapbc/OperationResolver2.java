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
 * @(#)OperationResolver2.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import com.sun.jbi.httpsoapbc.util.HttpGetStringUtil;
import com.sun.jbi.internationalization.Messages;

import java.util.HashSet;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Map;
import java.util.Set;

import javax.xml.ws.handler.MessageContext;

/**
 * Looks at an XML message content to decide which operation is being invoked/to be invoked.
 */
public class OperationResolver2 {
    
    private static final Logger mLogger = Messages.getLogger(OperationResolver2.class);
    private static final Messages mMessages = Messages.getMessages(OperationResolver2.class);
    
    public static OperationMetaData resolveOperation(MessageContext msgContext,
                                                     String endpointUrlContext,
                                                     Map metaDataMap)
            throws OperationResolveException {
        
        String httpEncoding = determineUrlEncoding(msgContext);
        if (OperationMetaData.HTTP_URL_ENCODING_UNSPECIFIED.equals(httpEncoding)) {
            String msg = mMessages.getString("HTTPBC-E00746.Unspecified_WSDL_HTTP_URL_encoding");
            throw new OperationResolveException(msg);
        }
        
        String operationLocation = determineOperationLocation(msgContext, endpointUrlContext);
        if (operationLocation == null) {
            String msg = mMessages.getString("HTTPBC-E00730.Operation_location_not_found",
                    new Object[] { retrieveQueryString(msgContext), endpointUrlContext } );
            throw new OperationResolveException(msg);
        }
        
        // Narrow candidate operations to the ones whose encoding matches the request's.
        Set operationMetaDataSet = new HashSet(metaDataMap.values());
        operationMetaDataSet = filterByEncoding(operationMetaDataSet, httpEncoding);
        if (operationMetaDataSet.isEmpty()) {
            String msg = mMessages.getString("HTTPBC-E00731.Operation_location_encoding_not_found", httpEncoding);
            throw new OperationResolveException(msg);
        }
                
        // Narrow candidate operations further, to the ones whose operation
        // location matches the request's.
        operationMetaDataSet = filterByOperationLocation(operationMetaDataSet, operationLocation, httpEncoding);
        if (operationMetaDataSet.size() != 1) {
            if (operationMetaDataSet.isEmpty()) {
                String msg = mMessages.getString(
                            "HTTPBC-E00732.Operation_filteredby_encoding_no_match",
                            new Object[] { operationLocation, httpEncoding } );
                throw new OperationResolveException(msg);
            } else {
                String msg = mMessages.getString(
                            "HTTPBC-E00733.Operation_multiples_match",
                            new Object[] { operationLocation, httpEncoding, operationMetaDataSet.size() } );
                throw new OperationResolveException(msg);
            }
        }
        
        OperationMetaData ret = (OperationMetaData) operationMetaDataSet.toArray()[0];
        if (mLogger.isLoggable(Level.FINE)) {
            mLogger.log(Level.FINE,
                        "Matching operation '" + ret.getHttpOperationLocation()
                        + "' for location '" + operationLocation
                        + "' and encoding '" + httpEncoding + "'");
        }
        return ret;
    }

    private static String retrieveQueryString(MessageContext msgContext) {
        String queryString = (String) msgContext.get(MessageContext.QUERY_STRING);
        if ("".equals(queryString) || queryString == null) {
            queryString = (String) msgContext.get(MessageContext.PATH_INFO);
        }
        if ("".equals(queryString)) {
            queryString = null;
        }
        return queryString;
    }
    
    private static String determineUrlEncoding(MessageContext msgContext) {
        // POST request exception. TODO: is it actually valid for a POST
        // operation to have a urlReplacement encoding?  The WSDL 1.1. HTTP
        // support is vague.
        String verb = String.valueOf(msgContext.get(MessageContext.HTTP_REQUEST_METHOD));
        if ("POST".equals(verb)) {
            return OperationMetaData.HTTP_URL_ENCODING_ENCODED;
        }
        
        String queryString = (String) msgContext.get(MessageContext.QUERY_STRING);
        if (queryString != null && !"".equals(queryString)) {
            return OperationMetaData.HTTP_URL_ENCODING_ENCODED;
        }
        queryString = (String) msgContext.get(MessageContext.PATH_INFO);
        if (queryString != null && !"".equals(queryString)) {
            return OperationMetaData.HTTP_URL_ENCODING_REPLACEMENT;
        }
        return OperationMetaData.HTTP_URL_ENCODING_UNSPECIFIED;
    }
    
    private static String determineOperationLocation(MessageContext msgContext, String contextPath) {
        StringBuffer location = new StringBuffer((String) msgContext.get(MessageContext.PATH_INFO));
        String operationLocation = null;
        if (location != null) {
            boolean denormalizeLocation = false;
            
            // This accounts for the possibility that the context is blank; e.g.:
            // http://localhost/foo/bar -> context is /foo/bar  -> /foo/bar/
            // http://localhost/        -> context is /         -> /
            // http://localhost         -> context is blank     -> /
            if (!contextPath.endsWith("/")) {
                contextPath = contextPath.concat("/");
            }
            // Path_info needs to be normalized too for the comparison
            if (location.length() == 0 || location.charAt(location.length() - 1) != '/') {
                location.append("/");
                // Set flag to remember if it was necessary to add a slash,
                // because we need to remove it later.
                denormalizeLocation = true;
            }
            // Context path must be equal to or an initial fragment of the path_info,
            // because path_info is supposed to be context + optional location.
            if (contextPath.length() <= location.length()
                    && location.subSequence(0, contextPath.length()).equals(contextPath)) {
                if (!denormalizeLocation) {
                    operationLocation = location.substring(contextPath.length());
                } else if (location.length() != contextPath.length()) {
                    operationLocation = location.substring(contextPath.length(), location.length() - 1);
                } else {
                    operationLocation = location.substring(contextPath.length());
                }
            }
        }
        return operationLocation;
    }
    
    private static Set filterByEncoding(Set<OperationMetaData> operationMetaDataSet, String httpEncoding) {
        Iterator<OperationMetaData> iter = operationMetaDataSet.iterator();
        while (iter.hasNext()) {
            OperationMetaData meta = iter.next();
            if (!httpEncoding.equals(meta.getHttpUrlEncoding())) {
                iter.remove();
            }
        }
        return operationMetaDataSet;
    }
    
    private static Set filterByOperationLocation(Set<OperationMetaData> operationMetaDataSet, String operationLocation, String httpEncoding) {
        if (OperationMetaData.HTTP_URL_ENCODING_ENCODED.equals(httpEncoding)) {
            Iterator<OperationMetaData> iter = operationMetaDataSet.iterator();
            while (iter.hasNext()) {
                OperationMetaData meta = iter.next();
                if (!meta.getHttpOperationLocation().equals(operationLocation)) {
                    iter.remove();
                }
            }
        }
        else if (OperationMetaData.HTTP_URL_ENCODING_REPLACEMENT.equals(httpEncoding)) {
            Iterator<OperationMetaData> iter = operationMetaDataSet.iterator();
            while (iter.hasNext()) {
                OperationMetaData meta = iter.next();
                try {
                    HttpGetStringUtil.extractNameValuePairs(operationLocation, meta.getHttpOperationLocation());
                } catch (HttpGetStringUtil.PatternMatchException e) {
                    iter.remove();
                }
            }
        }
        return operationMetaDataSet;
    }
}
