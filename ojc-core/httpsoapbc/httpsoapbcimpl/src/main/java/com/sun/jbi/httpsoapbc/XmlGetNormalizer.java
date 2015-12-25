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
 * @(#)XmlGetNormalizer.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.httpsoapbc;

import com.sun.jbi.httpsoapbc.util.DebugLog;
import com.sun.jbi.httpsoapbc.util.HttpGetStringUtil;
import com.sun.jbi.httpsoapbc.util.Util;
import com.sun.jbi.internationalization.Messages;
import com.sun.jbi.nms.wsdl11wrapper.HelperFactory;
import com.sun.jbi.nms.wsdl11wrapper.WrapperBuilder;
import com.sun.jbi.nms.wsdl11wrapper.WrapperProcessingException;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import javax.jbi.messaging.NormalizedMessage;
import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.MessageExchange;
import javax.wsdl.Message;
import javax.wsdl.Part;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.ws.handler.MessageContext;

import org.w3c.dom.Document;
import org.w3c.dom.Node;

/**
 *
 */
public class XmlGetNormalizer {

    private static final String SOAP_HEADERS_PROPERTY = "com.sun.jbi.headers.soap";
    private static final String HTTP_HEADERS_PROPERTY = "com.sun.jbi.headers.http";
    
    private static Messages mMessages = Messages.getMessages(XmlGetNormalizer.class);
    private static Logger mLog = Messages.getLogger(XmlGetNormalizer.class);
    private static TransformerFactory cTransformerFactory = TransformerFactory.newInstance();
    
    // The World Wide Web Consortium Recommendation states that UTF-8 should be used.
    // Not doing so may introduce incompatibilities
    // See http://java.sun.com/j2se/1.5.0/docs/api/java/net/URLDecoder.html#decode(java.lang.String,%20java.lang.String)
    private static final String URL_DECODE_ENCODING = "UTF-8";
    
    private final URLDecoder mUrlDecoder;
    private final DocumentBuilder mBuilder;
    private final WrapperBuilder wrapperBuilder;
    
    private boolean mIsHeaderCopyEnabled = false; // Feature disabled because BPELSE does not yet support it

    /** Creates a new instance of XmlNormalizer */
    public XmlGetNormalizer() throws MessagingException {
        try {
            wrapperBuilder = HelperFactory.createBuilder();        
        } catch (WrapperProcessingException ex) {
            throw new MessagingException("Failed to create wrapper builder", ex);
        }

        try {        
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            mBuilder = factory.newDocumentBuilder();
        } catch (ParserConfigurationException ex) {
            throw new MessagingException("Failed to create document builder", ex);
        }
        
        mUrlDecoder = new URLDecoder();
    }
    
    /**
     * Note that it is not safe to call this method concurrently on the same instance of XmlNormalizer,
     * so only one thread should work on a specific instance.
     *
     * To prevent a regression, it should be noted that this method was not safe as a static as neither the 
     * documentbuilder or builder instance are guaranteed to be thread safe.
     */
    private Document newDocument() throws ParserConfigurationException {
        Document doc = null;
        // As long as each instance is only be used by a single thread at the same time we do not want to synchronize
        // synchronized(mBuilder) {
            doc = mBuilder.newDocument();
        // }
            
        return doc;
    }
    
    public NormalizedMessage normalize(Object message,
                                       MessageExchange exchange,
                                       OperationMetaData meta,
                                       boolean inMsg,
                                       MessageContext context)
            throws MessagingException {
        
        NormalizedMessage normalMsg = exchange.createMessage();
        MessageContext msgContext;
        
        try {
            msgContext = (MessageContext) message;
        } catch (ClassCastException ex) {
            String msg = mMessages.getString(
                    "HTTPBC-E00797.Normalize_fail_wrong_type",
                    new Object[] { MessageContext.class.toString(), message.getClass().toString() });
            throw new MessagingException(msg, ex);
        }
        
        try {
            String name;
            Message msg;
            if (inMsg) {
                name = meta.getInMsgName();
                msg = meta.getInputMessage();
            } else {
                name = meta.getOutMsgName();
                msg = meta.getOutputMessage();
            }

            Document normalDoc = newDocument();
            wrapperBuilder.initialize(normalDoc, msg, name);

            // Decompose query string into a map of names and values
            // A urlEncoded GET request will have query string information, but
            // a urlReplacement GET request will have path info information.
            // The order of discovery below is important. A URL can have both
            // extra path information *and* a query.  If there's a query
            // (string), use that, and ignore the extra path information; that
            // extra information is part of the location value of the
            // WSDL-defined operation.
            String queryString = (String) msgContext.get(MessageContext.QUERY_STRING);
            if ("".equals(queryString) || queryString == null) {
                queryString = (String) msgContext.get(MessageContext.PATH_INFO);
            }
            Map<String, String> namesToValues = extractQuery(queryString, meta);

            // Generate the normalized message according to the input message
            // definition for the operation.  There must be a one-to-one
            // correspondence between message parts and name-value pairs; if
            // there are parts without a corresponding name-value pair, or
            // vice-versa, treat it as an exceptional case.
            Collection<Part> parts = msg.getParts().values();
            validateForCongruence(namesToValues, parts);
            if (!parts.isEmpty()) {
                for (Part p : parts) {
                    // Correspondence is already checked above, so no need
                    // to recheck keys/values again...
                    String localName = p.getName();
                    String value = namesToValues.get(localName);
                    // The data may (or may not be) XML. This transformation
                    // is costly, as it may fail, but necessary.  If the data
                    // is not XML, it is attached to the document as Text.
                    // If it is XML, it must be attached as an Element; its
                    // content is escaped if it is added as Text.
                    Node node = Util.textAsDomWithXMLCheck(value);
                    if (node == null) {
                        node = normalDoc.createTextNode(value == null ? "" : value);
                    } else if (node instanceof Document) {
                        node = ((Document) node).getDocumentElement();
                    }
                    NodeListImpl nodeList = new NodeListImpl();
                    nodeList.addItem(node);
                    wrapperBuilder.addPart(p.getName(), nodeList);
                }
            }


            normalDoc = wrapperBuilder.getResult();
            
            if (mIsHeaderCopyEnabled && context != null && inMsg) {
                processHTTPRequestHeaders(normalMsg, context);
            }

            if (mLog.isLoggable(Level.FINE)) {
                DebugLog.debugLog(mLog, Level.FINE, "Normalized message", normalDoc);
            }

            normalMsg.setContent(new DOMSource(normalDoc));
            return normalMsg;
        } catch (ParserConfigurationException tex) {
            throw new MessagingException("unable to obtain normalized message as a DOM document", tex);
        } catch (WrapperProcessingException ex) {
            String msg = mMessages.getString("HTTPBC-E00798.Normalize_fail");
            throw new MessagingException(msg, ex);
        } catch (TransformerConfigurationException e) {
            throw new MessagingException("unable to obtain normalized message as a DOM document", e);
        } catch (XMLStreamException e) {
            throw new MessagingException("unable to obtain normalized message as a DOM document", e);
	} catch (FactoryConfigurationError e) {
	    throw new MessagingException("unable to obtain normalized message as a DOM document", e);
	}
    }

    /**
     * Parses a URL fragment containing name-value information and returns them
     * in a map.  The urlEncodingType information in the opMetaData argument 
     * (@link opMetaData#getHttpUrlEncoding}) dictates how the URL fragment
     * is expected to be formatted.  If the specified encoding is
     * {@link OperationMetaData#HTTP_URL_ENCODING_ENCODED},
     * <code>queryString</code> must be a sequence of unencoded name-value
     * pairs in the form <code>n1=v1&n2=v2&...&nk=vk</code>.  If the encoding
     * is {@link OperationMetaData#HTTP_URL_ENCODING_REPLACEMENT}, then
     * <code>queryString</code> must be formatted as a URL's extra path
     * information, i.e., the <em>path</em> part as specified by RFC2396:
     *
     * <code>&lt;scheme&gt;://&lt;authority&gt;&lt;path&gt;?&lt;query&gt;#&lt;fragment&gt;</code>.
     *
     * @param queryString      The decoded URL
     * @param opMetaData       Information about the operation being serviced
     *
     * @return A name-value map
     *
     * @throws MessagingException if urlEncodingType is not recognized, or
     *                            if the queryString is malformed
     */
    protected final Map<String, String> extractQuery(String queryString,
                                                            OperationMetaData opMetaData)
        throws MessagingException {
        
        if (OperationMetaData.HTTP_URL_ENCODING_ENCODED.equals(opMetaData.getHttpUrlEncoding())) {
            return extractNameValuePairs(queryString);
            
        } else if (OperationMetaData.HTTP_URL_ENCODING_REPLACEMENT.equals(opMetaData.getHttpUrlEncoding())) {
            try {
                return HttpGetStringUtil.extractNameValuePairs(queryString, opMetaData.getHttpOperationLocation());
            } catch (HttpGetStringUtil.PatternMatchException ex) {
                String msg = mMessages.getString("HTTPBC-E00785.Url_replacement_failed_op_match",
                                                 new Object[] {
                                                     queryString,
                                                     opMetaData.getHttpOperationLocation()
                                                 });
                throw new MessagingException(msg);
            }
        } else {
            String msg = mMessages.getString("HTTPBC-E00756.Query_extract_encoding_unsupported",
                                             opMetaData.getHttpUrlEncoding());
            throw new MessagingException(msg);
        }
    }

    /**
     * Given a chain of name-value pairs in the format n1=v1&n2=v2&n3=v3&...&nk=vk,
     * creates a map where n1, n2... nk are keys, with respectively values
     * v1, v2... vk.
     *
     * @throws MessagingException if the name-value pair formatting is malformed.
     */
    protected final Map<String, String> extractNameValuePairs(String queryString)
            throws MessagingException {
        
        Map<String, String> nvMap = new HashMap<String, String>();
        if (queryString != null) {
            StringTokenizer nvTokenizer = new StringTokenizer(queryString, "&");
            while (nvTokenizer.hasMoreTokens()) {
                String nvPair = nvTokenizer.nextToken();
                StringTokenizer pairTokenizer = new StringTokenizer(nvPair, "=");
                if (pairTokenizer.countTokens() == 2) {
                    // THIS IS A TEMPORARY WORKAROUND -- It needs to be resolved ASAP.
                    // The issue is that query strings received thru JAX-WS
                    // appear to be url-encoded, but '=' characters are NOT
                    // escaped.  These characters disrupt the name-value parsing.
                    //
                    // The workaround implemented here is to double-decode.
                    //
                    // In the case where the request is transmited FROM the
                    // HTTP BC, the query string is encoded twice, once by the
                    // BC (see OutboundMessageProcessor), then ostensibly again
                    // by the JAX-WS layer.  In this case, the double decode
                    // is compensated for.
                    //
                    // In the case where the request does not originate from the
                    // BC, the query string will only possess the JAX-WS-applied
                    // encoding, which the double decode AT BEST does nothing
                    // (the content is idempotent and an additional decoding
                    // pass does not change it), and AT WORST mangles the data.
                    // In EITHER case, the presence of '=' characters which
                    // JAX-WS fails to encode will cause name-value pair parsing
                    // to fail anyway.
                    try {
                        String name = pairTokenizer.nextToken();
                        String value = pairTokenizer.nextToken();
                        nvMap.put(URLDecoder.decode(name, URL_DECODE_ENCODING),
                                URLDecoder.decode(value, URL_DECODE_ENCODING));
                    } catch (UnsupportedEncodingException e) {
                        String msg = mMessages.getString(
                                "HTTPBC-E00754.Query_encoding_unsupported",
                                URL_DECODE_ENCODING);
                        throw new MessagingException(msg, e);
                    }
                } else if (pairTokenizer.countTokens() > 0) {
                    String msg = mMessages.getString("HTTPBC-E00755.Query_invalid_segment", nvPair);
                    throw new MessagingException(msg);
                }
            }
        }
        return nvMap;
    }
    
    protected final void validateForCongruence(Map<String, String> namesToValues, Collection<Part> parts)
        throws MessagingException {
        
        final int partsSize = parts.size();
        final int nvSize = namesToValues.size();
        boolean valid = true;
        
        // Both name sets must have the same number of elements
        valid = (partsSize == nvSize);
        
        // Both sets' name elements must be equivalent
        if (valid) {
            int partsCount = 0;
            for (Part p : parts) {
                String partName = p.getName();
                if (namesToValues.containsKey(partName)) {
                    partsCount += 1;
                }
            }
            valid = (partsCount == partsSize);
        }
        
        if (!valid) {
            String msg = mMessages.getString("HTTPBC-W00700.Query_part_message_parts_mismatch");
            
            if (mLog.isLoggable(Level.WARNING)) {
                mLog.log(Level.WARNING, msg);
            }
            
            // Log a dump of the message parts and query parts.
            if (mLog.isLoggable(Level.FINE)) {
                StringBuffer detail = new StringBuffer(msg).append("\n");
                detail.append("Message parts dump:").append("\n");
                for (Part p : parts) {
                    String partName = p.getName();
                    detail.append(partName).append("\n");
                }
                detail.append("\nQuery parts dump:").append("\n");
                Set<String> keys = namesToValues.keySet();
                for (String key : keys) {
                    String value = namesToValues.get(key);
                    detail.append(key).append(": ").append(value != null ? value : "").append("\n");
                }
                mLog.log(Level.FINE, detail.toString());
            }
            throw new MessagingException(msg);
        }
    }
    
    // Copies HTTP request headers from the context, into the normalized
    // message.
    private void processHTTPRequestHeaders(NormalizedMessage normalMsg, MessageContext context) {
        Map httpHeadersProperty = (Map) normalMsg.getProperty(HTTP_HEADERS_PROPERTY);
        if (httpHeadersProperty == null) {
            httpHeadersProperty = new HashMap<String, String>();
            normalMsg.setProperty(HTTP_HEADERS_PROPERTY, httpHeadersProperty);
        }
        Map requestHeaders = (Map) context.get(MessageContext.HTTP_REQUEST_HEADERS);
        for (Iterator iter = requestHeaders.entrySet().iterator(); iter.hasNext(); ) {
            Map.Entry entry = (Map.Entry) iter.next();
            String key = (String) entry.getKey();
            // JAX-WS Packet implementation populates each header's value
            // as a single value contained in a singleton list.
            String value = (String) ((List) entry.getValue()).get(0);
            httpHeadersProperty.put(key, value);
        }
    }
}
