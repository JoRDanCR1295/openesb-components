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
 * @(#)CastorSupport.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.wsdl.model.xsd;

import com.sun.jbi.internationalization.Messages;
import com.sun.wsdl.model.Types;
import com.sun.wsdl.model.common.MessageManager;
//import com.sun.wsdl.model.common.model.QName;
import com.sun.wsdl.model.common.visitor.XMLParseVisitorException;
import com.sun.wsdl.model.extensions.ExtensibilityElement;
import com.sun.wsdl.model.uri.BaseURIResolver;

import java.io.Writer;
import java.util.Collection;
import java.util.Map;
import java.util.logging.Logger;
import javax.xml.namespace.QName;

import org.xml.sax.SAXException;

/**
 * Support for Castor XML Schema object model.
 *
 * @author Sun Microsystems
 * @version 
 */
public abstract class CastorSupport {

    /** The following unsupportedNode is meant for the xsd any support. At this point we 
     *  do not know how to handle xsd:any and hence we resolved to this failsafe mechanism.
     */
//    public static final String ANY = 
//            MessageManager.getManager(CastorSupport.class).getString("ANY");
    private static final Messages MESSAGES = 
            Messages.getMessages(CastorSupport.class);
    private static final Logger LOGGER = 
            Messages.getLogger(CastorSupport.class);
   
    /**
     * xsd <any> element
     */
    public static final String ANY_ELEMENT = "any";
    
    /**
     * xsd <anyAttribute> .
     */
    public static final String ANY_ATTRIBUTE = "anyAttribute";
    
    public static final String XSD_NAMESPACE = "http://www.w3.org/2001/XMLSchema";
    
    /** Castor support singleton */
    private static CastorSupport mCastorSupport = null;
    
    /** Current classLoader for Castor support singleton */
    private static ClassLoader mClassLoader = null;

    /**
     * Get Castor support from Enterprise Designer context.
     * @return CastorSupport    The castor support object.
     */
    public static synchronized CastorSupport getInstance() {
        mCastorSupport = loadImpl(null);
        if (mCastorSupport != null) {
            mClassLoader = mCastorSupport.getClass().getClassLoader();
        }

        return mCastorSupport;
    }
    
    /**
     * Get the Castor support using the given Class Loader.
     * @param   loader  Class Loader that can find the CastorSupport implementation class.
     * @return  AbstractCastorSuppport  The castor support object.
     */
    public static synchronized CastorSupport getInstance(ClassLoader loader) {
        if ((null == mCastorSupport) || (null == loader) || !loader.equals(mClassLoader)) {
            mCastorSupport = loadImpl(loader);
            mClassLoader = loader;
        }
        return mCastorSupport;
    }

    /** Loads the CastorSupport implementation class.
     * @param   loader  ClassLoader to use.
     * @return  CastorSupport implementing class.
     */
    private static CastorSupport loadImpl(ClassLoader loader) {
        String implClassName = null;
        CastorSupport acs = null;
        try {
            implClassName = System.getProperty("com.sun.wsdl.model.xsd.CastorSupport",
                                               "com.sun.wsdl.model.xsd.impl.CastorSupportImpl");
            Class implClass = null;
            if (loader != null) {
                implClass = Class.forName(implClassName, true, loader);
            } else {
                implClass = Class.forName(implClassName);
            }
            acs = (CastorSupport) implClass.newInstance();
        } catch (Exception e) {
            throw new XMLParseVisitorException(
                    MESSAGES.getString(
                    "CASTOR_SUPPORT.CANNOT_FIND_LOAD_CLASS_x",
                    new Object[]{implClassName}), e);
        }
        return acs;
    }
    
    /**
     * Parses an extensible element into an XMLSchema.
     * @param elem  the extensible element
     * @param map   map of other elements/schemas that may be referenced via include or
     *              import
     * @return the XMLSchema object instance or null if there are errors
     * @throws SAXException parsing errors
     */
    public abstract XMLSchema parseSchema(ExtensibilityElement elem, Map map)
            throws SAXException;
    
    /**
     * Parses an extensible element into an XMLSchema.
     * @param elem  the extensible element
     * @param res   URI resolver for imports.
     * @return the XMLSchema object instance or null if there are errors
     * @throws SAXException parsing errors
     */
    public abstract XMLSchema parseSchema(ExtensibilityElement elem, BaseURIResolver res)
            throws SAXException;
    
    /**
     * Parses an extensible element into an XMLSchema.
     * @param elem  the extensible element
     * @param res   URI resolver for imports.
     * @return the XMLSchema object instance or null if there are errors
     * @throws SAXException parsing errors
     */
    public abstract XMLSchema parseSchema(ExtensibilityElement elem, BaseURIResolver res, boolean validateSchema)
            throws SAXException;
    
    /**
     * Serializes an extensible element.
     * This can be used to serialize a &lt;schema&gt; element.
     * @param elem the element
     * @param writer the writer to serialize to
     */
    public abstract void serializeElement(ExtensibilityElement elem, Writer writer);
    
    /**
     * Gets the top-level simple types in the schema
     * @param   xmlSchema   The XML Schema
     * @return  The collection of top-level simple types
     * @since 5.0.5
     */
    public abstract Collection getSimpleTypes(XMLSchema xmlSchema);
    
    /**
     * Gets the top-level complex types in the schema.
     * @param xmlSchema the XML Schema
     * @return the collection of top-level complex types
     */
    public abstract Collection getComplexTypes(XMLSchema xmlSchema);
    
    /**
     * Gets the top-level element declarations in the schema.
     * @param xmlSchema the XML Schema
     * @return the collection of top-level element declarations
     */
    public abstract Collection getElementDecls(XMLSchema xmlSchema);

    /**
     * Gets the target namespace for the given schema object.
     * @param obj the schema object
     * @return the target namespace of the owning schema
     */
    public abstract String getTargetNamespace(Object obj);

    /**
     * Gets the name for the given schema object.
     * @param obj the schema object
     * @return the name
     */
    public abstract String getName(Object obj);

	/**
	 * Gets the type name for the given schema object.
	 * @param obj the schema object
	 * @return the QName. The QName has the namespce and the name values filled in.
	 */
	public abstract QName getType(Object obj);

	/**
	 * Gets the type name for the given schema object.
	 * @param obj the schema object
	 * @return the name
	 * @deprecate this method is buggy the prefixes might be hard coded and not appropriate. 
	 * use getType() instead
	 */
    public abstract String getXSDType(Object obj);

    /**
     * Get the short description of a xsd object. 
     * Used for tooltip etc.
     * @param obj the schema object
     * @return short description of a schema object.
     */
    public abstract String getShortDescription(Object obj);
    /**
     * Determines if the object is a known schema object.
     * @param obj the object
     * @return true if obj is a valid schema object
     */
    public abstract boolean isKnownSchemaObject(Object obj);

    /**
     * Gets the number of children (in a TreeModel) for the given object.
     * @param parent the object
     * @return the number of children
     */
    public abstract int getChildCount(Object parent);
    
    /**
     * Gets the child (in a TreeModel) for the given object.
     * @param parent the object
     * @param index the child index
     * @return the child
     */
    public abstract Object getChild(Object parent, int index);
    
    /**
     * Gets the number of children (in a TreeModel) for the given complex type.
     * @param obj the complex type
     * @return the number of children
     */
    public abstract int getComplexTypeChildCount(Object obj);
    
    /**
     * Gets the child (in a TreeModel) of a complex type.
     * @param obj the complex type
     * @param index the child index
     * @return the child
     */
    public abstract Object getComplexTypeChild(Object obj, int index);

    /**
     * Gets the number of children (in a TreeModel) for the given element.
     * @param obj the element declaration
     * @return the number of children
     */
    public abstract int getElementDeclChildCount(Object obj);
    
    /**
     * Gets the child (in a TreeModel) of an element declaration.
     * @param obj the element declaration
     * @param index the child index
     * @return the child
     */
    public abstract Object getElementDeclChild(Object obj, int index);
    
    /**
     * Gets the number of children (in a TreeModel) for the given group.
     * @param obj the group
     * @return the number of children
     */
    public abstract int getGroupChildCount(Object obj);
    
    /**
     * Gets the child (in a TreeModel) of a group.
     * @param obj the group
     * @param index the child index
     * @return the child
     */
    public abstract Object getGroupChild(Object obj, int index);
    
    /**
     * returns isSimpleType
     *
     * @param   obj     a object to be tested.
     * @return  returns <code>true</code> if object is a Castor SimpleType.
     * @since 5.0.5
     */
    public abstract boolean isSimpleType(Object obj);
    
    /**
     * returns isComplexType
     *
     * @param obj an <code>Object</code> value
     * @return a <code>boolean</code> value
     */
    public abstract boolean isComplexType(Object obj);
    
    /**
     * returns if it is GroupType
     *
     * @param obj an <code>Object</code> value
     * @return a <code>boolean</code> value
     */
    public abstract boolean isGroupType(Object obj);
    
    /**
     *  returns if it isElementType
     *
     * @param obj an <code>Object</code> value
     * @return a <code>boolean</code> value
     */
    public abstract boolean isElementType(Object obj);
    
    /**
     * returns if it is isAttributeType
     *
     * @param obj an <code>Object</code> value
     * @return a <code>boolean</code> value
     */
    public abstract boolean isAttributeType(Object obj);

    /**
     * returns if the attribute is optional
     *
     * @param obj an <code>Object</code> value
     * @return a <code>boolean</code> value
     */
    public abstract boolean isOptionalAttribute(Object obj);
    
    /**
     * returns by default a value of 1.
     *
     * @param obj an <code>Object</code> value
     * @return an <code>int</code> value
     */
    public abstract int getMin(Object obj);
    
    /**
     * returns by default a obj of 1.
     *
     * @param obj an <code>Object</code> obj
     * @return an <code>int</code> obj
     */
    public abstract int getMax(Object obj);
    
    /**
     * Returns Castor SimpleType Interface class.
     * @return  a <code>Class</code> value.
     * @since 5.0.5
     */
    public abstract Class getSimpleType();
    
    /**
     * Returns Castor ComplexType Interface class.
     * @return a <code>Class</code> value
     * @since 5.0.5
     */
    public abstract Class getComplexType();
    
    /**
     * Returns Castor ElementDecl Interface class.
     * @return a <code>Class</code> value
     * @since 5.0.5
     */
    public abstract Class getElementType();
    
    /**
     * Returns Castor Group Interface class.
     * @return a <code>Class</code> value.
     * @since 5.0.5
     */
    public abstract Class getGroupType();
    
    /**
     * Returns Castor AttributeDecl Interface class.
     * @return a <code>Class</code> value
     * @since 5.0.5
     */
    public abstract Class getAttributeType();
    
    /** Ensures that all inline Schemas under a types element have an equivalent ExtensibilityElement.
     *
     * @param   types   Types element with inline Schemas to be converted.
     * @since   5.1.0
     */
    public abstract void convertSchemaToExtensibilityElement(Types types);
    
}
