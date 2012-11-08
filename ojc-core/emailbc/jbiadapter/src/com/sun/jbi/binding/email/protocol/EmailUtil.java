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
 * @(#)EmailUtil.java
 *
 * Copyright 2004-2009 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.binding.email.protocol;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.jbi.messaging.NormalizedMessage;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.sun.jbi.binding.email.I18n;
import com.sun.jbi.binding.email.protocol.wsdl.EmailAddress;
import com.sun.jbi.binding.email.protocol.wsdl.EmailOperationInput;
import com.sun.jbi.binding.email.protocol.wsdl.SMTPAddress;
import com.sun.jbi.binding.email.protocol.wsdl.SMTPOperationInput;
import com.sun.jbi.common.qos.config.AppConfig;
import com.sun.jbi.common.qos.config.AppVar;
import com.sun.jbi.common.qos.config.ComponentConfig;

/**
 * @author Harry Liu (Harry.Liu@sun.com)
 */
public class EmailUtil {

    private static final Logger logger = Logger.getLogger(EmailUtil.class.getName());
    private static Map<String, String> nmPropsDef = new HashMap<String, String>();

    /**
     * Gets the mapping of wsdl field name to NM Property name
     * which is supported by this component.
     * @return A Map of <wsdlFieldName, NM Property Name>
     */
    public static Map<String, String> obsolete_getNMPropertiesDef() {
        if (nmPropsDef.size() > 0) {
            return nmPropsDef;
        }
        // nmPropsDef.put(wsdlFieldName, propertyName);
        nmPropsDef.put(EmailAddress.ATTR_EMAIL_HOSTNAME, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_SERVER_NAME);
        nmPropsDef.put(EmailAddress.ATTR_EMAIL_PORT, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_PORT);
        nmPropsDef.put(EmailAddress.ATTR_EMAIL_USERNAME, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_USERNAME);
        nmPropsDef.put(EmailAddress.ATTR_EMAIL_PASSWORD, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_PASSWORD);
        nmPropsDef.put(EmailAddress.ATTR_EMAIL_USESSL, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_USESSL);
        nmPropsDef.put(SMTPAddress.ATTR_SMTP_LOCATION, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_LOCATION);
        nmPropsDef.put(SMTPOperationInput.ATTR_SMTP_CHARSET, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_CHARSET);
        nmPropsDef.put(SMTPOperationInput.ATTR_SMTP_USE_TYPE, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_USE);
        nmPropsDef.put(SMTPOperationInput.ATTR_SMTP_ENCODING_STYLE, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_ENCODING_STYLE);
        nmPropsDef.put(EmailOperationInput.ATTR_EMAIL_SUBJECT, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_SUBJECT);
        nmPropsDef.put(EmailOperationInput.ATTR_EMAIL_FROM, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_FROM);
        nmPropsDef.put(EmailOperationInput.ATTR_EMAIL_TO, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_TO);
        nmPropsDef.put(EmailOperationInput.ATTR_EMAIL_CC, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_CC);
        nmPropsDef.put(EmailOperationInput.ATTR_EMAIL_BCC, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_BCC);
        //nmPropsDef.put(EmailOperationInput.ATTR_EMAIL_BODY, EmailBCConstants.NM_PROPERTY_EMAIL_SMTP_BODY);
        return nmPropsDef;
    }

    /**
     *
     * Application Configuration
     * NM Properties
     * Application Variables
     * @return
     * @throws
     */
    public static String resolveValue(String fieldValue, String wsdlFieldName, AppConfig appConfig,
            String nmPropertyName, NormalizedMessage nmMsg, ComponentConfig config) throws ApplicationVariableNotDefinedException {
        String value = fieldValue;

        value = resolveAppConfigValue(value, wsdlFieldName, appConfig);

        value = resolveNMPropertyValue(value, nmPropertyName, nmMsg);

        value = resolveAppVarsValue(value, config);

        return value;
    }

    /**
     * @param fieldValue
     * @param wsdlFieldName
     * @param appConfig
     * @return
     * @throws Exception
     */
    public static String resolveAppConfigValue(String fieldValue, String wsdlFieldName,
            AppConfig appConfig) {
        String retValue = fieldValue;

        // wsdlFieldName is also used as AppConfig's property name in jbi.xml
        if (null != appConfig) {
            String appConfigPropertyValue = appConfig.getProperty(wsdlFieldName).getValue();
            if (null != appConfigPropertyValue) {
                // wsdlFieldName is supported as one AppConfig property AND has value
                retValue = appConfigPropertyValue;
            }
        }

        // Note the AppVariables may come from wsdlFieldValue, or AppConfig, or NM Properties
        //retValue = resolveAppVarsValue(retValue);

        return retValue;
    }

    /**
     * @param fieldValue
     * @param nmPropertyName
     * @param nmMsg
     * @return
     * @throws Exception
     */
    public static String resolveNMPropertyValue(String fieldValue, String nmPropertyName,
            NormalizedMessage nmMsg) {
        String retValue = fieldValue;

        // resolve NM Property if applicable
        if (null != nmPropertyName && nmMsg != null) {
            String nmPropertyValue = (String) nmMsg.getProperty(nmPropertyName);
            if (null != nmPropertyValue) {
                // The specific NM Property is used
                retValue = nmPropertyValue;
            }
        }

        // Note the AppVariables may come from fieldValue, or AppConfig, or NM Properties
        //retValue = resolveAppVarsValue(retValue);

        return retValue;
    }

    /**
     * @param fieldValue
     * @return
     * @throws Exception
     */
    public static String resolveAppVarsValue(String fieldValue, ComponentConfig config)
            throws ApplicationVariableNotDefinedException {
        String retValue = fieldValue;

        if (null == retValue || 0 == retValue.length()) {
            return retValue;
        }

        // resolve Application Variables if applicable
        Vector<String> appVarsFound = findAppVars(fieldValue);
        if (appVarsFound.size() <= 0) {
            return retValue;
        }

        for (String appVar : appVarsFound) {
            AppVar appVarDef = config.getAppVar(appVar.substring(2, appVar.length() - 1));
            if (null != appVarDef) {
                String appVarValue = appVarDef.getValue();
                if (null != appVarValue) {
                    retValue = retValue.replace(appVar, appVarValue);
                }
            }
        }

        // check again
        appVarsFound = findAppVars(retValue);
        // still there are AppVars ... ${...}
        if (appVarsFound.size() > 0) {
            // but nothing was replaced/resolved
            if (retValue.equals(fieldValue)) {
                // they must be undefined App Vars
                throw new ApplicationVariableNotDefinedException(I18n.loc("EMAILBC-7012: Application Variables {0} are not defined but they are used in value [{1}].", appVarsFound, fieldValue));
            } else {
                // need to resolve again (recursively)
                retValue = resolveAppVarsValue(retValue, config);
            }
        }

        return retValue;
    }

    /**
     * @param fieldValue
     * @return
     */
    public static Vector<String> findAppVars(String fieldValue) {
        String regex = "(\\$\\{[\\w\\.\\-@#%/]+\\})"; // ${var1}
        //String escapedRegex = "(?:[^\\$]+|[^\\$]+(?:\\$\\$)+)" + regex; // ${var1} but $${non-var}
        //example: Matcher matcher = Pattern.compile(escapedRegex).matcher("subject=\"Escape $ by use of $$ like ANT does: Match ${Var1}, not match $${Var2}, match $$${Var3}, not match $$$${Var4}.\"");
        Pattern pattern = Pattern.compile(regex);
        Matcher matcher = pattern.matcher(fieldValue);
        Vector<String> appVars = new Vector<String>();
        String appVar = "";
        while (matcher.find()) {
            appVar = matcher.group(1);
            if (!appVars.contains(appVar)) {
                appVars.add(appVar);
            }
        }
        return appVars;
    }

    /**
     * Create a new document.
     * 
     * @return
     * @throws ParserConfigurationException
     */
    public static Document newDocument() throws ParserConfigurationException {

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        Document document = factory.newDocumentBuilder().newDocument();
        return document;
    }

    /**
     * Parse the input string to build a document.
     * 
     * @param input
     * @param charset null means system default
     * @return
     * @throws IOException
     * @throws SAXException
     * @throws ParserConfigurationException
     */
    public static Document buildDocument(String input, String charset)
            throws IOException, SAXException, ParserConfigurationException {

        byte[] bytes = null;
        if (null != charset) {
            bytes = input.getBytes(charset);
        } else {
            bytes = input.getBytes();
        }
        return buildDocument(bytes);
    }

    /**
     * Parse the byte array to build a document.
     * 
     * @param bytes
     * @return
     * @throws IOException
     * @throws SAXException
     * @throws ParserConfigurationException
     */
    public static Document buildDocument(byte[] bytes)
            throws IOException, SAXException, ParserConfigurationException {

        InputStream inputStream = new ByteArrayInputStream(bytes);
        return buildDocument(inputStream);
    }

    /**
     * Parse the input stream to build a document.
     * 
     * @param inputStream
     * @return
     * @throws IOException
     * @throws SAXException
     * @throws ParserConfigurationException
     */
    public static Document buildDocument(InputStream inputStream)
            throws IOException, SAXException, ParserConfigurationException {

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        Document document = factory.newDocumentBuilder().parse(inputStream);
        return document;
    }

    /**
     * Parse the input source to build a document.
     * 
     * @param inputSource
     * @return
     * @throws IOException
     * @throws SAXException
     * @throws ParserConfigurationException
     */
    public static Document buildDocument(InputSource inputSource)
            throws IOException, SAXException, ParserConfigurationException {

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setNamespaceAware(true);
        Document document = factory.newDocumentBuilder().parse(inputSource);
        return document;
    }

    /**
     * Transform the source and return the result node.
     * 
     * @param source
     * @return
     * @throws TransformerException
     */
    public static Node transformToNode(Source source)
            throws TransformerException {
        DOMResult result = new DOMResult();
        if (null == source) {
            return result.getNode();
        }

        if (source instanceof StreamSource) {
            try {
                StreamSource streamSource = (StreamSource) source;
                InputStream inputStream = streamSource.getInputStream();
                Reader reader = streamSource.getReader();
                if (null != inputStream) {
                    inputStream.reset();
                }
                if (null != reader) {
                    reader.reset();
                }
            } catch (Exception e) {
                //ignore
            }
        }

        TransformerFactory factory = TransformerFactory.newInstance();
        Transformer transformer = factory.newTransformer();
        transformer.transform(source, result);

        return result.getNode();
    }

    /**
     * Get the owner document of the node.
     * @param node
     * @return
     */
    public static Document getDocument(Node node) {
        if (null == node) {
            return null;
        }

        Document doc = null;
        if (node instanceof Document) {
            doc = (Document) node;
        } else {
            doc = node.getOwnerDocument();
        }
        return doc;
    }

    /**
     * Build document from input stream, then transform document's
     * root element into string with "text" output method.
     * In case any error, return the string which the input stream represents.
     * 
     * For the OutputKeys properties, value null means to keep the default setting.
     * 
     * With settings:
     * 
     * "UTF-8",  //encoding
     * Boolean.FALSE, //indent
     * "text",  //method
     * Boolean.TRUE,  //omitXmlDeclaration
     * 
     * @param inputStream
     * @param contentType null means default (which is "text/plain")
     * @return
     */
    public static String transformToTextPlain(
            InputStream inputStream, String contentType) {

        return transformToText(inputStream,
                null, //cdataSectionElements
                null, //doctypePublic
                null, //doctypeSystem
                "UTF-8", //encoding
                Boolean.FALSE, //indent
                (null == contentType ? "text/plain" : contentType), //mediaType
                "text", //method
                Boolean.TRUE, //omitXmlDeclaration
                null, //standalone
                null);  //version
    }

    /**
     * Build document from string text, then transform document's
     * root element into string with "text" output method.
     * In case any error, the original text will be returned.
     * 
     * For the OutputKeys properties, value null means to keep the default setting.
     * 
     * With settings:
     * 
     * "UTF-8",  //encoding
     * Boolean.FALSE, //indent
     * "text",  //method
     * Boolean.TRUE,  //omitXmlDeclaration
     * 
     * @param text
     * @param charset null means system default
     * @param contentType null means default (which is "text/plain")
     * @return
     */
    public static String transformToTextPlain(
            String text, String charset, String contentType) {

        return transformToText(text, charset,
                null, //cdataSectionElements
                null, //doctypePublic
                null, //doctypeSystem
                "UTF-8", //encoding
                Boolean.FALSE, //indent
                (null == contentType ? "text/plain" : contentType), //mediaType
                "text", //method
                Boolean.TRUE, //omitXmlDeclaration
                null, //standalone
                null);  //version
    }

    /**
     * Transform the node into string.
     * 
     * For the OutputKeys properties, value null means to keep the default setting.
     * 
     * With settings:
     * 
     * "UTF-8",  //encoding
     * Boolean.FALSE, //indent
     * "text",  //method
     * Boolean.TRUE,  //omitXmlDeclaration
     * 
     * @param node
     * @param contentType null means default (which is "text/plain")
     * @return
     * @throws IOException 
     * @throws TransformerException 
     */
    public static String transformToTextPlain(Node node, String contentType)
            throws TransformerException, IOException {

        return transformToText(node,
                null, //cdataSectionElements
                null, //doctypePublic
                null, //doctypeSystem
                "UTF-8", //encoding
                Boolean.FALSE, //indent
                (null == contentType ? "text/plain" : contentType), //mediaType
                "text", //method
                Boolean.TRUE, //omitXmlDeclaration
                null, //standalone
                null);  //version
    }

    /**
     * Build document from input stream, then transform document's
     * root element into string with "html" output method.
     * In case any error, return the string which the input stream represents.
     * 
     * For the OutputKeys properties, value null means to keep the default setting.
     * 
     * With settings:
     * 
     * "UTF-8",  //encoding
     * Boolean.TRUE, //indent
     * "text/html",  //mediaType
     * "html",  //method
     * Boolean.TRUE,  //omitXmlDeclaration
     * "4.0");  //version
     * 
     * @param inputStream
     * @return
     */
    public static String transformToTextHtml(InputStream inputStream) {

        return transformToText(inputStream,
                null, //cdataSectionElements
                null, //doctypePublic
                null, //doctypeSystem
                "UTF-8", //encoding
                Boolean.TRUE, //indent
                "text/html", //mediaType
                "html", //method
                Boolean.TRUE, //omitXmlDeclaration
                null, //standalone
                "4.0");  //version
    }

    /**
     * Build document from string text, then transform document's
     * root element into string with "html" output method.
     * In case any error, the original text will be returned.
     * 
     * For the OutputKeys properties, value null means to keep the default setting.
     * 
     * With settings:
     * 
     * "UTF-8",  //encoding
     * Boolean.TRUE, //indent
     * "text/html",  //mediaType
     * "html",  //method
     * Boolean.TRUE,  //omitXmlDeclaration
     * "4.0");  //version
     * 
     * @param text
     * @param charset null means system default
     * @return
     */
    public static String transformToTextHtml(
            String text, String charset) {

        return transformToText(text, charset,
                null, //cdataSectionElements
                null, //doctypePublic
                null, //doctypeSystem
                "UTF-8", //encoding
                Boolean.TRUE, //indent
                "text/html", //mediaType
                "html", //method
                Boolean.TRUE, //omitXmlDeclaration
                null, //standalone
                "4.0");  //version
    }

    /**
     * Transform the node into string.
     * 
     * For the OutputKeys properties, value null means to keep the default setting.
     * 
     * With settings:
     * 
     * "UTF-8",  //encoding
     * Boolean.TRUE, //indent
     * "text/html",  //mediaType
     * "html",  //method
     * Boolean.TRUE,  //omitXmlDeclaration
     * "4.0");  //version
     * 
     * @param node
     * @return
     * @throws IOException 
     * @throws TransformerException 
     */
    public static String transformToTextHtml(Node node)
            throws TransformerException, IOException {

        return transformToText(node,
                null, //cdataSectionElements
                null, //doctypePublic
                null, //doctypeSystem
                "UTF-8", //encoding
                Boolean.TRUE, //indent
                "text/html", //mediaType
                "html", //method
                Boolean.TRUE, //omitXmlDeclaration
                null, //standalone
                "4.0");  //version
    }

    /**
     * Build document from input stream, then transform document's
     * root element into string with "xml" output method.
     * In case any error, return the string which the input stream represents.
     * 
     * For the OutputKeys properties, value null means to keep the default setting.
     * 
     * With settings:
     * 
     * "UTF-8",  //encoding
     * Boolean.TRUE, //indent
     * text/xml",  //mediaType
     * "xml",  //method
     * Boolean.FALSE,  //omitXmlDeclaration
     * "1.0");  //version
     * 
     * @param inputStream
     * @return
     */
    public static String transformToTextXml(InputStream inputStream) {

        return transformToText(inputStream,
                null, //cdataSectionElements
                null, //doctypePublic
                null, //doctypeSystem
                "UTF-8", //encoding
                Boolean.TRUE, //indent
                "text/xml", //mediaType
                "xml", //method
                Boolean.FALSE, //omitXmlDeclaration
                null, //standalone
                "1.0");  //version
    }

    /**
     * Build document from string text, then transform document's
     * root element into string with "xml" output method.
     * In case any error, the original text will be returned.
     * 
     * For the OutputKeys properties, value null means to keep the default setting.
     * 
     * With settings:
     * 
     * "UTF-8",  //encoding
     * Boolean.TRUE, //indent
     * text/xml",  //mediaType
     * "xml",  //method
     * Boolean.FALSE,  //omitXmlDeclaration
     * "1.0");  //version
     * 
     * @param text
     * @param charset null means system default
     * @return
     */
    public static String transformToTextXml(
            String text, String charset) {

        return transformToText(text, charset,
                null, //cdataSectionElements
                null, //doctypePublic
                null, //doctypeSystem
                "UTF-8", //encoding
                Boolean.TRUE, //indent
                "text/xml", //mediaType
                "xml", //method
                Boolean.FALSE, //omitXmlDeclaration
                null, //standalone
                "1.0");  //version
    }

    /**
     * Transform the node into string.
     * 
     * For the OutputKeys properties, value null means to keep the default setting.
     * 
     * With settings:
     * 
     * "UTF-8",  //encoding
     * Boolean.TRUE, //indent
     * text/xml",  //mediaType
     * "xml",  //method
     * Boolean.FALSE,  //omitXmlDeclaration
     * "1.0");  //version
     * 
     * @param node
     * @return
     * @throws IOException 
     * @throws TransformerException 
     */
    public static String transformToTextXml(Node node)
            throws TransformerException, IOException {

        return transformToText(node,
                null, //cdataSectionElements
                null, //doctypePublic
                null, //doctypeSystem
                "UTF-8", //encoding
                Boolean.TRUE, //indent
                "text/xml", //mediaType
                "xml", //method
                Boolean.FALSE, //omitXmlDeclaration
                null, //standalone
                "1.0");  //version
    }

    /**
     * Build document from input stream, then transform document's
     * root element into string.
     * In case any error, return the string which the input stream represents.
     * 
     * For the OutputKeys properties, value null means to keep the default setting.
     * 
     * @param inputStream
     * @param cdataSectionElements
     * @param doctypePublic
     * @param doctypeSystem
     * @param encoding default is UTF-8 for "xml" method
     * @param indent default value is "no" for "xml" method, "yes" for "html" method
     * @param mediaType default value is "text/xml" for "xml" method, "text/html" for "html", "text/plain" for "text"
     * @param method "xml" | "html" | "text" | expanded name.
     * @param omitXmlDeclaration "yes" | "no".
     * @param standalone "yes" | "no".
     * @param version default value is "1.0" for "xml" method, "4.0" for "html"
     * @return
     */
    public static String transformToText(InputStream inputStream,
            String cdataSectionElements,
            String doctypePublic,
            String doctypeSystem,
            String encoding, // UTF-8, UTF-16 for "xml" method
            Boolean indent, // "yes" | "no". The default value is "no".
            String mediaType, // default value is "text/xml" for "xml" method, "text/html" for "html", "text/plain" for "text"
            String method, // "xml" | "html" | "text" | expanded name.
            Boolean omitXmlDeclaration, // "yes" | "no".
            Boolean standalone, //"yes" | "no".
            String version // default value is "1.0" for "xml" method, "4.0" for "html"
            ) {

        Document doc;
        try {
            doc = buildDocument(inputStream);
            Element element = doc.getDocumentElement();
            return transformToText(element,
                    cdataSectionElements,
                    doctypePublic,
                    doctypeSystem,
                    encoding, // UTF-8, UTF-16 for "xml" method
                    indent, // "yes" | "no". The default value is "no".
                    mediaType, // default value is "text/xml" for "xml" method, "text/html" for "html", "text/plain" for "text"
                    method, // "xml" | "html" | "text" | expanded name.
                    omitXmlDeclaration, // "yes" | "no".
                    standalone, //"yes" | "no".
                    version);  // default value is "1.0" for "xml" method, "4.0" for "html"
        } catch (TransformerException ex) {
            Logger.getLogger(EmailUtil.class.getName()).log(Level.SEVERE, "Could not transform to text", ex);
        } catch (IOException ex) {
            Logger.getLogger(EmailUtil.class.getName()).log(Level.SEVERE, "Could not transform to text", ex);
        } catch (SAXException ex) {
            Logger.getLogger(EmailUtil.class.getName()).log(Level.SEVERE, "Could not transform to text", ex);
        } catch (ParserConfigurationException ex) {
            Logger.getLogger(EmailUtil.class.getName()).log(Level.SEVERE, "Could not transform to text", ex);
        }
        // give up and return the original content
        return toText(inputStream, null);
    }

    /**
     * Build document from text, then transform document's
     * root element into string. 
     * In case any error, the original text will be returned.
     * 
     * For the OutputKeys properties, value null means to keep the default setting.
     * 
     * @param text
     * @param charset null means system default
     * @param cdataSectionElements
     * @param doctypePublic
     * @param doctypeSystem
     * @param encoding default is UTF-8 for "xml" method
     * @param indent default value is "no" for "xml" method, "yes" for "html" method
     * @param mediaType default value is "text/xml" for "xml" method, "text/html" for "html", "text/plain" for "text"
     * @param method "xml" | "html" | "text" | expanded name.
     * @param omitXmlDeclaration "yes" | "no".
     * @param standalone "yes" | "no".
     * @param version default value is "1.0" for "xml" method, "4.0" for "html"
     * @return
     */
    public static String transformToText(String text, String charset,
            String cdataSectionElements,
            String doctypePublic,
            String doctypeSystem,
            String encoding, // UTF-8, UTF-16 for "xml" method
            Boolean indent, // "yes" | "no". The default value is "no".
            String mediaType, // default value is "text/xml" for "xml" method, "text/html" for "html", "text/plain" for "text"
            String method, // "xml" | "html" | "text" | expanded name.
            Boolean omitXmlDeclaration, // "yes" | "no".
            Boolean standalone, //"yes" | "no".
            String version // default value is "1.0" for "xml" method, "4.0" for "html"
            ) {

        if (null == text) {
            return text;
        }

        try {
            Document doc = buildDocument(text, charset);
            Element element = doc.getDocumentElement();
            return transformToText(element,
                    cdataSectionElements,
                    doctypePublic,
                    doctypeSystem,
                    encoding, // UTF-8, UTF-16 for "xml" method
                    indent, // "yes" | "no". The default value is "no".
                    mediaType, // default value is "text/xml" for "xml" method, "text/html" for "html", "text/plain" for "text"
                    method, // "xml" | "html" | "text" | expanded name.
                    omitXmlDeclaration, // "yes" | "no".
                    standalone, //"yes" | "no".
                    version);  // default value is "1.0" for "xml" method, "4.0" for "html"
        } catch (TransformerException e) {
            I18n.finest(logger, "EMAILBC-3001: Failed to transform text: {0}. The original text is returned: {1}", e,
                    e.toString(), text);
        } catch (IOException e) {
            I18n.finest(logger, "EMAILBC-3001: Failed to transform text: {0}. The original text is returned: {1}", e,
                    e.toString(), text);
        } catch (SAXException e) {
            I18n.finest(logger, "EMAILBC-3001: Failed to transform text: {0}. The original text is returned: {1}", e,
                    e.toString(), text);
        } catch (ParserConfigurationException e) {
            I18n.finest(logger, "EMAILBC-3001: Failed to transform text: {0}. The original text is returned: {1}", e,
                    e.toString(), text);
        }

        // give up and return the original text
        return text;
    }

    /**
     * Transform the node into string.
     * 
     * For the OutputKeys properties, value null means to keep the default setting.
     * 
     * @param node
     * @param cdataSectionElements
     * @param doctypePublic
     * @param doctypeSystem
     * @param encoding default is UTF-8 for "xml" method
     * @param indent default value is "no" for "xml" method, "yes" for "html" method
     * @param mediaType default value is "text/xml" for "xml" method, "text/html" for "html", "text/plain" for "text"
     * @param method "xml" | "html" | "text" | expanded name.
     * @param omitXmlDeclaration "yes" | "no".
     * @param standalone "yes" | "no".
     * @param version default value is "1.0" for "xml" method, "4.0" for "html"
     * @return
     * @throws TransformerException 
     * @throws IOException 
     */
    public static String transformToText(Node node,
            String cdataSectionElements,
            String doctypePublic,
            String doctypeSystem,
            String encoding, // UTF-8, UTF-16 for "xml" method
            Boolean indent, // default value is "no" for "xml" method, "yes" for "html" method
            String mediaType, // default value is "text/xml" for "xml" method, "text/html" for "html", "text/plain" for "text"
            String method, // "xml" | "html" | "text" | expanded name.
            Boolean omitXmlDeclaration, // "yes" | "no".
            Boolean standalone, //"yes" | "no".
            String version // default value is "1.0" for "xml" method, "4.0" for "html"
            )
            throws TransformerException, IOException {

        if (null == node) {
            return ""; // or null?
        }

        if (node instanceof Text) {
            if ("text".equalsIgnoreCase(method)) {
                //don't do transformation ... bypass transformer's double CR issue
                return ((Text) node).getTextContent();
            }
        }

        TransformerFactory factory = TransformerFactory.newInstance();
        Transformer transformer = factory.newTransformer();

        if (null != cdataSectionElements) {
            transformer.setOutputProperty(OutputKeys.CDATA_SECTION_ELEMENTS, cdataSectionElements);
        }
        if (null != doctypePublic) {
            transformer.setOutputProperty(OutputKeys.DOCTYPE_PUBLIC, doctypePublic);
        }
        if (null != doctypeSystem) {
            transformer.setOutputProperty(OutputKeys.DOCTYPE_SYSTEM, doctypeSystem);
        }
        if (null != encoding) {
            transformer.setOutputProperty(OutputKeys.ENCODING, encoding);
        }
        if (null != indent) {
            transformer.setOutputProperty(OutputKeys.INDENT, (indent.booleanValue() ? "yes" : "no"));
        }
        if (null != mediaType) {
            transformer.setOutputProperty(OutputKeys.MEDIA_TYPE, mediaType);
        }
        if (null != method) {
            transformer.setOutputProperty(OutputKeys.METHOD, method);
        }
        if (null != omitXmlDeclaration) {
            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, (omitXmlDeclaration.booleanValue() ? "yes" : "no"));
        }
        if (null != standalone) {
            transformer.setOutputProperty(OutputKeys.STANDALONE, (standalone.booleanValue() ? "yes" : "no"));
        }
        if (null != version) {
            transformer.setOutputProperty(OutputKeys.VERSION, version);
        }

        Source source = new DOMSource(node);
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        StreamResult result = new StreamResult(baos);

        transformer.transform(source, result);

        baos.flush();
        baos.close();

        return baos.toString();
    }

    /**
     * Transform each item of the node list into 
     * the corresponding item of the string list.
     * 
     * For the OutputKeys properties, value null means to keep the default setting.
     * 
     * @param nodeList
     * @param cdataSectionElements
     * @param doctypePublic
     * @param doctypeSystem
     * @param encoding default is UTF-8 for "xml" method
     * @param indent default value is "no" for "xml" method, "yes" for "html" method
     * @param mediaType default value is "text/xml" for "xml" method, "text/html" for "html", "text/plain" for "text"
     * @param method "xml" | "html" | "text" | expanded name.
     * @param omitXmlDeclaration "yes" | "no".
     * @param standalone "yes" | "no".
     * @param version default value is "1.0" for "xml" method, "4.0" for "html"
     * @return
     * @throws TransformerException
     * @throws IOException
     */
    public static List<String> transformToTextList(NodeList nodeList,
            String cdataSectionElements,
            String doctypePublic,
            String doctypeSystem,
            String encoding, // UTF-8, UTF-16 for "xml" method
            Boolean indent, // default value is "no" for "xml" method, "yes" for "html" method
            String mediaType, // default value is "text/xml" for "xml" method, "text/html" for "html", "text/plain" for "text"
            String method, // "xml" | "html" | "text" | expanded name.
            Boolean omitXmlDeclaration, // "yes" | "no".
            Boolean standalone, //"yes" | "no".
            String version // default value is "1.0" for "xml" method, "4.0" for "html"
            )
            throws TransformerException, IOException {
        List<String> list = new ArrayList<String>();
        if (null == nodeList) {
            return list;
        }

        for (int i = 0; i < nodeList.getLength(); i++) {
            list.add(transformToText(nodeList.item(i),
                    cdataSectionElements,
                    doctypePublic,
                    doctypeSystem,
                    encoding,
                    indent,
                    mediaType,
                    method,
                    omitXmlDeclaration,
                    standalone,
                    version));
        }

        return list;
    }

    /**
     * Read the input stream into byte array,
     * then convert it into string.
     * 
     * @param inputStream
     * @param charset null means system default 
     * @return
     */
    public static String toText(InputStream inputStream, String charset) {

        byte[] bytes = new byte[1024];
        String str = "";
        int bytesRead = 0;
        StringBuffer wholeBuffer = new StringBuffer();
        try {
            while ((bytesRead = inputStream.read(bytes)) != -1) {
                if (null != charset) {
                    str = new String(bytes, 0, bytesRead, charset);
                } else {
                    str = new String(bytes, 0, bytesRead);
                }
                wholeBuffer.append(str);
            }
        } catch (IOException ie) {
            // return whatever we've got
        }

        return wholeBuffer.toString();
    }

    // for test usage only
    public static void main(String[] args) throws Exception {
        /*
        // test1:
        String input1 = "number 2 > number 1";
        String input2 = "<element>number 2 > number 1</element>";
        String input3 = "&lt;element&gt;number 2 > number 1&lt;/element&gt;";
        //
        String outputPlain1 = transformToTextPlain(input1, null, null);
        String outputPlain2 = transformToTextPlain(input2, null, null);
        String outputPlain3 = transformToTextPlain(input3, null, null);
        String outputHtml1 = transformToTextHtml(input1, null);
        String outputHtml2 = transformToTextHtml(input2, null);
        String outputHtml3 = transformToTextHtml(input3, null);
        String outputXml1 = transformToTextXml(input1, null);
        String outputXml2 = transformToTextXml(input2, null);
        String outputXml3 = transformToTextXml(input3, null);
        //
        System.out.println("\r\n outputPlain1:\r\n" + outputPlain1);
        System.out.println("\r\n outputPlain2:\r\n" + outputPlain2);
        System.out.println("\r\n outputPlain3:\r\n" + outputPlain3);
        System.out.println("\r\n outputHtml1:\r\n" + outputHtml1);
        System.out.println("\r\n outputHtml2:\r\n" + outputHtml2);
        System.out.println("\r\n outputHtml3:\r\n" + outputHtml3);
        System.out.println("\r\n outputXml1:\r\n" + outputXml1);
        System.out.println("\r\n outputXml2:\r\n" + outputXml2);
        System.out.println("\r\n outputXml3:\r\n" + outputXml3);
         */
        //
        // test2:
        //String in = "number 2 is greater than number 1";
        //String in = "number 2 is > number 1";
        //String in = "<anyTag>number 2 is greater than number 1</anyTag>";
        //String in = "<anyTag>number 2 is > number 1</anyTag>";
        //String in = "first line\n second line";
        String in = "first line\r\n second line";
        /*
        String in = "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">" +
        "<html>" +
        "<head>" +
        "</head>" +
        "<body bgcolor=\"#ffffff\" text=\"#000000\">" +
        "This is a test message. All lines below are in color blue.<br>" +
        "<font color=\"#3333ff\">A line with <b>bold</b> text.<br>" +
        "&lt;anyTag&gt;text inside a pair of closing tag&lt;/anyTag&gt;<br>" +
        "A line with \"greater\" sign &gt;.<br>" +
        "A line with amp sign &amp;.<br>" +
        "A line with nested tags" +
        "&lt;a&gt;&lt;b&gt;BBBBBBBB&lt;/b&gt;&lt;c&gt;CCCCCCC&lt;/c&gt;&lt;/a&gt;</font><br>" +
        "</body>" +
        "</html>";
         */
        String inPlain = transformToTextPlain(in, null, null);
        String inHtml = transformToTextHtml(in, null);
        String inXml = transformToTextXml(in, null);
        System.out.println("\r\n inPlain:\r\n" + inPlain);
        System.out.println("\r\n inHtml:\r\n" + inHtml);
        System.out.println("\r\n inXml:\r\n" + inXml);
        Document document = newDocument();
        Element message = document.createElement("message");
        Element part = document.createElement("part");
        Node text = document.createTextNode(in);
        message.appendChild(part);
        part.appendChild(text);
        String textMessage = transformToTextPlain(message, null);
        String textPart = transformToTextPlain(part, null);
        String textText = transformToTextPlain(text, null);
        String textTextPlain = transformToTextPlain(textText, null, null);
        System.out.println("\r\n textMessage:\r\n" + textMessage);
        System.out.println("\r\n textPart:\r\n" + textPart);
        System.out.println("\r\n textText:\r\n" + textText);
        System.out.println("\r\n back to textTextPlain:\r\n" + textTextPlain);
        String htmlMessage = transformToTextHtml(message);
        String htmlPart = transformToTextHtml(part);
        String htmlText = transformToTextHtml(text);
        String htmlTextHtml = transformToTextHtml(htmlText, null);
        System.out.println("\r\n htmlMessage:\r\n" + htmlMessage);
        System.out.println("\r\n htmlPart:\r\n" + htmlPart);
        System.out.println("\r\n htmlText:\r\n" + htmlText);
        System.out.println("\r\n back to htmlTextHtml:\r\n" + htmlTextHtml);
        String xmlMessage = transformToTextXml(message);
        String xmlPart = transformToTextXml(part);
        String xmlText = transformToTextXml(text);
        String xmlTextXml = transformToTextXml(xmlText, null);
        System.out.println("\r\n xmlMessage:\r\n" + xmlMessage);
        System.out.println("\r\n xmlPart:\r\n" + xmlPart);
        System.out.println("\r\n xmlText:\r\n" + xmlText);
        System.out.println("\r\n back to xmlTextXml:\r\n" + xmlTextXml);
        //
    }
}
