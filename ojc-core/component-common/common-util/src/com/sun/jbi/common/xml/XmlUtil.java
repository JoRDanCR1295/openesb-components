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
 * @(#)XmlUtil.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.common.xml;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringWriter;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.wsdl.Definition;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.schema.Schema;
import javax.wsdl.xml.WSDLReader;
import javax.xml.transform.Source;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentFragment;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import com.sun.jbi.common.util.I18n;
import com.sun.wsdl4j.ext.DeferredActionRegistry;
import com.sun.wsdl4j.ext.WSDL4JExt;

/**
 * XML utility methods for Common-Util.
 * @author Kevan Simpson
 */
public class XmlUtil {
	private static Logger mLogger = Logger.getLogger(XmlUtil.class.getName());
    private static XmlResourcePool mPool = null;
    
    private XmlUtil() {}
    
    /**
     * Creates a {@link DocumentFragment} containing all element nodes in the specified list.
     * 
     * @param nodes A list of nodes to add to fragment
     * @return a <code>DocumentFragment</code> or <code>null</code>.
     */
    public static DocumentFragment createDocumentFragment(NodeList nodes) {
    	if (nodes != null) {
    		DocumentFragment frag = null;
    		for (int i = 0, n = nodes.getLength(); i < n; i++) {
    			Node node = nodes.item(i);
    			if (node instanceof Element) {
    				if (frag == null) { // must be owner doc, new doc fails
    					frag = node.getOwnerDocument().createDocumentFragment();
    				}
    				frag.appendChild(node);
    			}
    		}

    		return frag;
    	}

    	return null;
    }
    
    /**
     * Return common-util's singleton {@link XmlResourcePool}.
     * @return a pool of XML resources.
     */
    public static XmlResourcePool getXmlResourcePool() {
    	if (mPool == null) {
    		mPool = new XmlResourcePoolImpl(10);
    	}
    	return mPool;
    }
    
    /**
     * Sets common-util's singleton {@link XmlResourcePool}.
     * @param pool A pool of XML resources.
     */
    public static void setXmlResourcePool(XmlResourcePool pool) {
    	mPool = pool;
    }
    
    /**
     * Creates a new DOM Document from a pool of builders.
     * @return a new <code>Document</code>.
     */
    public static Document newDocument() {
    	XmlResource xml = null;
    	try {
    		xml = getXmlResourcePool().acquireXmlResource();
    		return xml.getDocumentBuilder().newDocument();
    	}
    	finally {
    		getXmlResourcePool().releaseXmlResource(xml);
    	}
    }

    /**
     * Formats the specified xml source and returns an xml string.
     * <b>Note:</b> If the specified <code>Source</code> is an instance of
     * <code>StreamSource</code>, it will be {@link #reset(Source)} after
     * the xml string is generated.
     * 
     * @param src The xml source object.
     * @return Formatted xml or an empty string if formatting fails.
     * @see #reset(Source)
     */
    public static String print(Source src) {
    	XmlResource rsrc = null;
        
        try {
        	rsrc = getXmlResourcePool().acquireXmlResource();
            StringWriter writer = new StringWriter();
            StreamResult dest = new StreamResult(writer);
            rsrc.getTransformer().transform(src, dest);
            reset(src); // in case it's a StreamSource
            return writer.toString();
        } 
        catch (Exception e) {
        	if (mLogger.isLoggable(Level.FINE)) {
        		mLogger.fine("UTIL-3005: Generation of xml string failed: "+ 
        					 e.getMessage());
        	}
            return "";
        }
        finally {
        	getXmlResourcePool().releaseXmlResource(rsrc);
        }
    }

    /**
     * Reads a wsdl4j {@link Definition} from the specified {@link File},
     * with the option to pass parameters to 
     * {@link WSDL4JExt#newWSDLReader(org.xml.sax.EntityResolver, com.sun.wsdl4j.ext.DeferredActionRegistry, java.util.Map, java.util.Map).
     * 
     * @param file The wsdl file.
     * @param readerParams The optional parameters to {@link WSDL4JExt#newWSDLReader(org.xml.sax.EntityResolver, com.sun.wsdl4j.ext.DeferredActionRegistry, java.util.Map, java.util.Map).
     * @return a wsdl <code>Definition</code> or <code>null</code> if the file is <code>null</code>.
     * @throws FileNotFoundException if the specified file does not exist.
     * @throws WSDLException if an error occurs reading wsdl.
     */
    public static Definition readWsdl(File file, Object... readerParams) throws Exception {
        if (file != null) {
            WSDLReader reader = readParams(readerParams);
            Definition def = reader.readWSDL(
                    file.toURI().toString(), new InputSource(new FileReader(file)));
            return def;
        }

        return null;
    }

    /**
     * Reads a wsdl4j {@link Definition} from the specified XXX,
     * with the option to pass parameters to 
     * {@link WSDL4JExt#newWSDLReader(org.xml.sax.EntityResolver, com.sun.wsdl4j.ext.DeferredActionRegistry, java.util.Map, java.util.Map).
     * 
     * @param file The wsdl file.
     * @param readerParams The optional parameters to {@link WSDL4JExt#newWSDLReader(org.xml.sax.EntityResolver, com.sun.wsdl4j.ext.DeferredActionRegistry, java.util.Map, java.util.Map).
     * @return a wsdl <code>Definition</code> or <code>null</code> if the file is <code>null</code>.
     * @throws FileNotFoundException if the specified file does not exist.
     * @throws WSDLException if an error occurs reading wsdl.
     */
    public static Definition readWsdl(InputStream stream, Object... readerParams) throws Exception {
        if (stream != null) {
            WSDLReader reader = readParams(readerParams);
            Definition def = reader.readWSDL(null, new InputSource(stream));
            return def;
        }

        return null;
    }

    private static WSDLReader readParams(Object... params) {
        WSDLReader reader = null;
        int len = (params == null) ? 0 : params.length;
        EntityResolver er = null;
        DeferredActionRegistry reg = null;
        Map<String, Definition> wsdlCache = null;
        Map<String, Schema> xsdCache = null;
        
        switch (len) {
            case 4: {
                if (params[3] instanceof Map) {
                    xsdCache = (Map<String, Schema>) params[3];
                }
            }
            case 3: {
                if (params[2] instanceof Map) {
                    wsdlCache = (Map<String, Definition>) params[2];
                }
            }
            case 2: {
                if (params[1] instanceof DeferredActionRegistry) {
                    reg = (DeferredActionRegistry) params[1];
                }
            }
            case 1: {
                if (params[0] instanceof EntityResolver) {
                    er = (EntityResolver) params[0];
                }
            }
            default: {
                reader = WSDL4JExt.newWSDLReader(er, reg, wsdlCache, xsdCache);
            }
        }
        
        return reader;
    }
    
    /**
     * Converts the specified file into a <code>Document</code>.
     * @param file A file containing xml data.
     * @return A DOM Document.
     * @throws IllegalArgumentException if the specified file does not exist.
     * @throws Exception if an error occurs reading stream or parsing xml.
     */
	public static Document readXml(File file) throws Exception {
		if (file == null) {
			return null;
		}
		else if (!file.exists()) {
			throw new IllegalArgumentException(I18n.loc(
					"UTIL-6004: Descriptor file does not exist: {0}", 
					file.getAbsolutePath()));
		}

		Document expectedDoc = null;
		XmlResource xml = null;
		try {
			xml = XmlUtil.getXmlResourcePool().acquireXmlResource();
			expectedDoc = xml.getDocumentBuilder().parse(file);
		}
		finally {
			XmlUtil.getXmlResourcePool().releaseXmlResource(xml);
			xml = null;
		}

		return expectedDoc;
	}

    /**
     * Converts the specified {@link InputSource} into a <code>Document</code>.
     * @param src An <code>InputSource</code> containing xml data.
     * @return A DOM Document.
     * @throws Exception if an error occurs reading stream or parsing xml.
     */
	public static Document readXml(InputSource src) throws Exception {
		if (src == null) {
			return null;
		}

		Document expectedDoc = null;
		XmlResource xml = null;
		try {
			xml = XmlUtil.getXmlResourcePool().acquireXmlResource();
			expectedDoc = xml.getDocumentBuilder().parse(src);
		}
		finally {
			XmlUtil.getXmlResourcePool().releaseXmlResource(xml);
			xml = null;
		}

		return expectedDoc;
	}

    /**
     * Converts the specified stream into a <code>Document</code>.
     * @param stream An input stream containing xml data.
     * @return A DOM Document.
     * @throws Exception if an error occurs reading stream or parsing xml.
     */
	public static Document readXml(InputStream stream) throws Exception {
		if (stream == null) {
			return null;
		}
		
		Document expectedDoc = null;
		XmlResource xml = null;
		try {
			xml = XmlUtil.getXmlResourcePool().acquireXmlResource();
			expectedDoc = xml.getDocumentBuilder().parse(stream);
		}
		finally {
			XmlUtil.getXmlResourcePool().releaseXmlResource(xml);
			xml = null;
		}

		return expectedDoc;
	}

	/**
	 * If the specified <code>Source</code> is an instance of <code>StreamSource</code>,
	 * then its internal input stream and reader are reset to allow successive reads
	 * of this <code>Source</code> instance.  If the specified <code>Source</code> is not
	 * a <code>StreamSource</code>, no action is performed.
	 * 
	 * @param src The specified <code>Source</code>.
	 * @throws IOException if an error occurs resetting the input stream or reader.
	 */
	public static void reset(Source src) throws IOException {
	    if (src instanceof StreamSource) {
    	    try {
                StreamSource stream = (StreamSource) src;
                InputStream inputStream = stream.getInputStream();
                if (inputStream != null) {
                    inputStream.reset();
                }
                Reader reader = stream.getReader();
                if (reader != null) {
                    reader.reset();
                }
    	    }
            catch (IOException e) {
                String msg = I18n.loc("UTIL-6011: Failed to reset StreamSource: {0}", e.getMessage());
                mLogger.log(Level.WARNING, msg);
                if (mLogger.isLoggable(Level.FINE)) {
                    // log exception at fine
                    mLogger.log(Level.FINE, "UTIL-3007: StreamSource reset failure stacktrace is below", e);
                }
                throw e;
            }
	    }
	}
	
	/**
	 * Converts the specified <code>Source</code> into a <code>DOMSource</code>
	 * to allow for multiple reads of the xml tree.
	 * 
	 * @param src The <code>Source</code> to convert.
	 * @return a <code>DOMSource</code> instance or <code>null</code>.
	 * @throws Exception if an error occurs converting the specified <code>Source</code>.
	 */
    public static DOMSource toDOMSource(Source src) throws Exception {
    	if (src instanceof DOMSource) {
    		return (DOMSource) src;
    	}

    	XmlResource rsrc = null;
        try {
        	rsrc = getXmlResourcePool().acquireXmlResource();
            DOMResult result = new DOMResult(newDocument());
            rsrc.getTransformer().transform(src, result);   
            return new DOMSource((Document) result.getNode());
        } 
        catch (Exception e) {
        	String msg = I18n.loc("UTIL-6010: Failed to convert to DOMSource: {0}", e.getMessage());
        	mLogger.log(Level.WARNING, msg);
        	if (mLogger.isLoggable(Level.FINE)) {
        		// log exception at fine
        		mLogger.log(Level.FINE, "UTIL-3006: DOMSource convert failure stacktrace is below", e);
        	}
            throw e;
        }
        finally {
        	getXmlResourcePool().releaseXmlResource(rsrc);
        }
    }
}
