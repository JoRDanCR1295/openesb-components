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
 * @(#)CocoXsdBuilderSpec.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.coco.xsdbuilder;

import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * COCO XSD builder-specific information and context.
 *
 * @author  Noel Ang, Jun Xu
 * @version $Revision: 1.3 $
 */
public class CocoXsdBuilderSpec {

    /**
     * Location of the COBOL Copybook. Value is a string containing file path
     */
    public static final String COPYBOOK_LOCATION = "CopybookLocation";
    /**
     * The character encoding of the COBOL Copybook document
     */ 
    public static final String COPYBOOK_CHAR_ENCODING = "CopybookCharEncoding";
    /**
     * Location of the XSD being built. Value is a string containing file path 
     */
    public static final String XSD_LOCATION = "XsdLocation";
    /**
     * The target namespace of the XSD being built
     */
    public static final String TARGET_NAMESPACE = "TargetNamespace";
    /**
     * Flag: whether or not to ignore copybook content beyond column 72
     */ 
    public static final String IGNORE_CONTENT_BEYOND_COLUMN72 =
        "IgnoreExtraContent";
    /**
     * Flag: whether or not to check item names against reserved words
     */ 
    public static final String CHECK_NAMES_FOR_RESERVED_WORDS =
        "CheckNamesForReservedWords";
    /**
     * The character encoding for DISPLAY usage
     */ 
    public static final String DISPLAY_CHAR_ENCODING = "DisplayCharEncoding";
    /**
     * The character encoding for DISPLAY1 usage
     */ 
    public static final String DISPLAY1_CHAR_ENCODING = "Display1CharEncoding";
    /**
     * Pre-decoding character coding
     */
    public static final String PREDECODE_CHAR_CODING = "PreDecodeCharCoding";
    /**
     * Post-encoding character coding
     */
    public static final String POSTENCODE_CHAR_CODING = "PostEncodeCharCoding";
    
    private Map<String, Object> mProp;
    
    /**
     * Create a COBOL Copybook XSD builder specification
     */
    public CocoXsdBuilderSpec() {
        mProp = Collections.synchronizedMap(new HashMap<String, Object>());
    }

    public String getCopybookLocation() {
        return (String) getProperty(COPYBOOK_LOCATION);
    }
    
    public void setCopybookLocation(String location) {
        setProperty(COPYBOOK_LOCATION, location);
    }
    
    public String getCopybookCharEncoding() {
        return (String) getProperty(COPYBOOK_CHAR_ENCODING);
    }
    
    public void setCopybookCharEncoding(String encoding) {
        setProperty(COPYBOOK_CHAR_ENCODING, encoding);
    }
    
    public String getXsdLocation() {
        return (String) getProperty(XSD_LOCATION);
    }
    
    public void setXsdLocation(String location) {
        setProperty(XSD_LOCATION, location);
    }
    
    public String getTargetNamespace() {
        return (String) getProperty(TARGET_NAMESPACE);
    }
    
    public void setTargetNamespace(String targetNamespace) {
        setProperty(TARGET_NAMESPACE, targetNamespace);
    }
    
    public boolean getIgnoreContentBeyondCol72() {
        Boolean boolObj = ((Boolean) mProp.get(IGNORE_CONTENT_BEYOND_COLUMN72));
        if (boolObj != null) {
            return boolObj.booleanValue();
        }
        return false;
    }
    
    public void setIgnoreContentBeyondCol72(boolean val) {
        setProperty(IGNORE_CONTENT_BEYOND_COLUMN72, new Boolean(val));
    }
    
    public boolean getCheckNamesForReservedWords() {
        Boolean boolObj = ((Boolean) mProp.get(CHECK_NAMES_FOR_RESERVED_WORDS));
        if (boolObj != null) {
            return boolObj.booleanValue();
        }
        return false;
    }
    
    public void setCheckNamesForReservedWords(boolean val) {
        setProperty(CHECK_NAMES_FOR_RESERVED_WORDS, new Boolean(val));
    }
    
    public String getDisplayCharEncoding() {
        return (String) getProperty(DISPLAY_CHAR_ENCODING);
    }
    
    public void setDisplayCharEncoding(String val) {
        setProperty(DISPLAY_CHAR_ENCODING, val);
    }
    
    public String getDisplay1CharEncoding() {
        return (String) getProperty(DISPLAY1_CHAR_ENCODING);
    }
    
    public void setDisplay1CharEncoding(String val) {
        setProperty(DISPLAY1_CHAR_ENCODING, val);
    }
    
    public String getPreDecodeCharCoding() {
        return (String) getProperty(PREDECODE_CHAR_CODING);
    }
    
    public void setPreDecodeCharCoding(String coding) {
        setProperty(PREDECODE_CHAR_CODING, coding);
    }
    
    public String getPostEncodeCharCoding() {
        return (String) getProperty(POSTENCODE_CHAR_CODING);
    }
    
    public void setPostEncodeCharCoding(String coding) {
        setProperty(POSTENCODE_CHAR_CODING, coding);
    }
    
    /**
     * returns an iterator to iterate through all the properties
     * of a specific OtdContextSpec
     *
     * @return Iterator list of property names
     */
    public Iterator getPropertyNames() {
        return Collections.unmodifiableMap(mProp).keySet().iterator();
    }
    
    /**
     * Tests if the specified property is presented.
     * 
     * @param name the property name
     * @return true if the property is there
     */
    public boolean hasProperty(String name) {
        return mProp.containsKey(name);
    }
    
    /**
     * returns Object identifying the value of a property
     *
     * @param name the name of a property
     *
     * @return Object identifying the value of a property
     */
    public Object getProperty(String name) {
        return mProp.get(name);
    }
    
    /**
     * sets Object identifying the value of a property
     *
     * @param name the name of a property
     * @param value the value of a property
     */
    public void setProperty(String name, Object value) {
        mProp.put(name, value);
    }

    @Override
    public String toString() {
        return mProp.toString();
    }
}
