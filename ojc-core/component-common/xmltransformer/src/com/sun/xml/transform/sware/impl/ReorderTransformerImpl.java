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
 * @(#)ReorderTransformerImpl.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.xml.transform.sware.impl;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import javax.xml.namespace.QName;
import javax.xml.transform.ErrorListener;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.URIResolver;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;

import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.sun.xml.transform.sware.InvalidSchemaException;
import com.sun.xml.transform.sware.MissingSchemaInfoException;
import com.sun.xml.transform.sware.NotNamespaceAwareException;
import com.sun.xml.transform.sware.SwareDOMImplementation;
import com.sun.xml.transform.sware.SwareDOMImplementation.Policies;
import com.sun.xml.transform.sware.schema.SwareComplexType;
import com.sun.xml.transform.sware.schema.SwareElement;
import com.sun.xml.transform.sware.schema.SwareFlatComplexType;
import com.sun.xml.transform.sware.schema.SwareSchemaException;
import com.sun.xml.transform.sware.schema.SwareType;
import com.sun.xml.transform.sware.schema.SwareTypeSystem;

/**
 * The instance of this class provides the service that reorders the elements
 * in a DOM document based on a governing schema.
 *  
 * @author Jun Xu
 * @since 6.0
 * @version 
 */
public class ReorderTransformerImpl extends Transformer {

    private final DOMImplementation mDOMImpl;
    private final SwareTypeSystem mSchemaTypeSystem;
    private final Policies mPolicies;
    private final Map<String, Object> mParameters =
        new HashMap<String, Object>();
    private final Logger mLogger = Logger.getLogger(this.getClass().getName());
    
    private ErrorListener mErrorListener;
    
    /**
     * Constructs from a schema instance and a policies instance.
     * 
     * @param domImpl an instance of DOMImplementation used for creating
     *                new DOM documents if needed. The value of this parameter
     *                must not be null. 
     * @param schemaTypeSystem an XML schema type system that governs
     *              the reordering transformation. The value of this parameter
     *              must not be null.
     * @param policies policies that govern conflict resolution and fail over
     *                 handling etc. 
     */
    ReorderTransformerImpl(DOMImplementation domImpl,
            SwareTypeSystem schemaTypeSystem, Policies policies) {
        if (domImpl == null) {
            throw new NullPointerException("no DOMImplementation.");
        }
        if (schemaTypeSystem == null) {
            throw new NullPointerException("no CastorSwareTypeSystemImpl.");
        }
        mDOMImpl = domImpl;
        mSchemaTypeSystem = schemaTypeSystem;
        mPolicies = policies;
    }
    
    /**
     * Reorders the elements based on the schema this transformer is aware of.
     * The source must be a DOMSource and the result must be a DOM result for
     * now.  The DOM document in the outputTarget is a different document
     * instance except the root element of the source document is a simple
     * type, simple content, or anyType element without xsi:type attribute.
     * In this case, the DOM element in the outputTarget is same as the
     * source document.
     *
     * @param xmlSource XML source (must be an instance of DOMSource and cannot
     *                  be null)
     * @param outputTarget output target (currently only DOMResult is supported)
     *  
     * @see Transformer#transform(javax.xml.transform.Source,
     *          javax.xml.transform.Result)
     */
    public void transform(Source xmlSource, Result outputTarget)
            throws TransformerException {
        if (!(xmlSource instanceof DOMSource)) {
            throw new IllegalArgumentException("Must be a DOM source.");
        }
        if (!(outputTarget instanceof DOMResult)) {
            throw new IllegalArgumentException("Must be a DOM result.");
        }
        if (!(((DOMSource) xmlSource).getNode() instanceof Element)) {
            throw new IllegalArgumentException(
                    "DOMSorter: DOM source must contain an DOM element.");
        }
        Element docElement = (Element) ((DOMSource) xmlSource).getNode();;
        if (docElement == null) {
            //document element is null, so there is nothing to reorder
            ((DOMResult) outputTarget).setNode(null);
            return;
        }
        
        QName xsiType = DOMUtil.getXsiType(docElement);
        SwareType docType;
        if (xsiType != null) {
            //has xsi:type attribute
            try {
                docType = mSchemaTypeSystem.findType(xsiType);
            } catch (SwareSchemaException e) {
                error(new InvalidSchemaException("Invalid schema.", e));
                return;
            }
            if (docType == null) {
                error(new MissingSchemaInfoException(
                        "Cannot find the XML type definition in"
                        + " the schema type system: " + xsiType));
                return;
            }
        } else if (getParameter(SwareDOMImplementation.Parameters.XML_TYPE)
                != null) {
            QName docTypeName =
                (QName) getParameter(
                        SwareDOMImplementation.Parameters.XML_TYPE); 
            try {
                docType =
                    mSchemaTypeSystem.findType(docTypeName);
            } catch (SwareSchemaException e) {
                error(new InvalidSchemaException("Invalid schema.", e));
                return;
            }
            if (docType == null
                    && !SwareTypeSystem.XSD_NS_URI.equals(
                            docTypeName.getNamespaceURI())) {
                //XML type not found and it is not built-in type
                error(new MissingSchemaInfoException(
                        "Cannot find the XML type definition in"
                        + " the schema type system: " + docTypeName));
            }
        } else {
            String docURI = docElement.getNamespaceURI();
            String docLocal = docElement.getLocalName();
            if (docURI == null && docLocal == null) {
                throw new NotNamespaceAwareException(
                        "The source DOM document must be namespace aware.");
            }
            QName docElementQName =
                new QName(docElement.getNamespaceURI(),
                    docElement.getLocalName());
            SwareElement docElemDecl;
            try {
                docElemDecl = mSchemaTypeSystem.findElement(docElementQName);
            } catch (SwareSchemaException e) {
                error(new InvalidSchemaException("Invalid schema.", e));
                return;
            }
            if (docElemDecl == null) {
                error(new MissingSchemaInfoException(
                        "Cannot find the element declaration in"
                        + " the schema type system: " + docElementQName));
                return;
            }
            try {
                docType = docElemDecl.getType();
            } catch (SwareSchemaException e) {
                error(new InvalidSchemaException(e));
                return;
            }
        }
        if (docType == null || !docType.isComplexType()
                || ((SwareComplexType) docType).isSimpleContent()) {
            //Don't need to reorder if the type is anyType, simple type or
            //simple content complex type
            ((DOMResult) outputTarget).setNode(docElement);
            return;
        }
        SwareFlatComplexType fct;
        try {
            fct = mSchemaTypeSystem.getFlatComplexType(
                    (SwareComplexType) docType);
            
            //Check if sorting is needed
            DOMSorter sorter;
            Boolean preChecking = (Boolean)
                getParameter(SwareDOMImplementation.Parameters.PRE_CHECKING);
            if (preChecking == null || preChecking.booleanValue()) {
                sorter = new DOMSorter(this, fct, docElement, null);
                if (!sorter.checkSortNeeded()) {
                    ((DOMResult) outputTarget).setNode(docElement);
                    return;
                }
            }
            
            Boolean inplace = (Boolean)
                getParameter(SwareDOMImplementation.Parameters.IN_PLACE); 
            Element tgtDocElem = docElement;
            if (inplace != null && !inplace.booleanValue()) {
                String qualiName =
                    docElement.getPrefix() != null
                        && docElement.getPrefix().length() > 0 ?
                                docElement.getPrefix() + ":"
                                    + docElement.getLocalName() :
                                        docElement.getLocalName();
                Document targetDocument =
                    mDOMImpl.createDocument(docElement.getNamespaceURI(),
                            qualiName, null);
                tgtDocElem = targetDocument.getDocumentElement();
                DOMUtil.copyAttributes(docElement, tgtDocElem);
            }
                
            sorter =
                new DOMSorter(this, fct, docElement, tgtDocElem);
            sorter.setInplace(inplace == null || inplace.booleanValue());
            sorter.sort();
            ((DOMResult) outputTarget).setNode(tgtDocElem);
        } catch (SwareSchemaException e) {
            error(new InvalidSchemaException("Invalid schema.", e));
            return;
        }
    }

    /**
     * Sets additional parameters for the transformer.  The parameters
     * currently recognized by this transformer are:
     * <table border='1' cellpadding='3'>
     * <tr>
     * <th>name</th>
     * <th>description</th>
     * <th>value</th>
     * <th>comment</th>
     * </tr>
     * <tr>
     * <td valign='top' rowspan='1' colspan='1'>
     * <code>xmltype</code></td>
     * <td valign='top' rowspan='1' colspan='1'>The qualified name of the XML type of the document element to be sorted</code></td>
     * <td valign='top' rowspan='1' colspan='1'>must be an instance of <code>javax.xml.namespace.QName</code></td>
     * <td valign='top' rowspan='1' colspan='1'>If this parameter is set, then the XML type specified will be used to govern the reordering</td>
     * </tr>
     * </table> 
     * @see Transformer#setParameter(java.lang.String, java.lang.Object)
     */
    public void setParameter(String name, Object value) {
        mParameters.put(name, value);
    }

    /**
     * @see Transformer#getParameter(java.lang.String)
     */
    public Object getParameter(String name) {
        return mParameters.get(name);
    }

    /**
     * @see Transformer#clearParameters()
     */
    public void clearParameters() {
        mParameters.clear();
    }

    /**
     * @see Transformer#setURIResolver(javax.xml.transform.URIResolver)
     */
    public void setURIResolver(URIResolver resolver) {
        //No need to implement this method so far
    }

    /**
     * @see Transformer#getURIResolver()
     */
    public URIResolver getURIResolver() {
        //No need to implement this method so far
        return null;
    }

    /**
     * @see Transformer#setOutputProperties(java.util.Properties)
     */
    public void setOutputProperties(Properties oformat) {
        //No need to implement this method so far
    }

    /**
     * @see Transformer#getOutputProperties()
     */
    public Properties getOutputProperties() {
        //No need to implement this method so far
        return null;
    }

    /**
     * @see Transformer#setOutputProperty(java.lang.String, java.lang.String)
     */
    public void setOutputProperty(String name, String value)
            throws IllegalArgumentException {
        //No need to implement this method so far
    }

    /**
     * @see Transformer#getOutputProperty(java.lang.String)
     */
    public String getOutputProperty(String name)
            throws IllegalArgumentException {
        //No need to implement this method so far
        return null;
    }

    /**
     * @see Transformer#setErrorListener(javax.xml.transform.ErrorListener)
     */
    public void setErrorListener(ErrorListener listener)
            throws IllegalArgumentException {
        mErrorListener = listener;
    }

    /**
     * @see Transformer#getErrorListener()
     */
    public ErrorListener getErrorListener() {
        return mErrorListener;
    }

    Policies getPolicies() {
        return mPolicies;
    }
    
    SwareTypeSystem getSwareTypeSystem() {
        return mSchemaTypeSystem;
    }
    
    /**
     * Gives a warning.
     * 
     * @param e an instance of TransformerException
     * @throws TransformerException transformer exception
     */
    void warning(String warning)
            throws TransformerException {
        mLogger.warning(warning);
        if (mErrorListener != null) {
            mErrorListener.warning(new TransformerException(warning));
        }
    }
    
    /**
     * Gives a warning.
     * 
     * @param e an instance of TransformerException
     * @throws TransformerException transformer exception
     */
    void warning(TransformerException e)
            throws TransformerException {
        mLogger.warning(e.toString());
        if (mErrorListener != null) {
            mErrorListener.warning(e);
        } else {
            throw e;
        }
    }
    
    /**
     * Complains an error.
     * 
     * @param e an instance of TransformerException
     * @throws TransformerException transformer exception
     */
    void error(TransformerException e)
            throws TransformerException {
        mLogger.severe(e.toString());
        if (mErrorListener != null) {
            mErrorListener.error(e);
        } else {
            throw e;
        }
    }
}
