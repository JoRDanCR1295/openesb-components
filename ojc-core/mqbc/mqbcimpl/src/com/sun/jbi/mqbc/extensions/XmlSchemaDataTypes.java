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
 */

/*
 * @(#)$Id: XmlSchemaDataTypes.java,v 1.2 2008/12/16 21:48:13 noel_ang Exp $
 *
 * Copyright 2008-2011 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.mqbc.extensions;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import javax.xml.XMLConstants;
import javax.xml.namespace.QName;

/**
 * An enumeration of a subset of primitive and derivative XML Schema Datatypes.
 * 
 * @author Noel.Ang@sun.com
 */
public enum XmlSchemaDataTypes {
    NOTYPE             (new QName(MQConstants.NS_URI_MQ, "NoType")),
    ANYSPECIALTYPE     (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "anySimpleType")),
    DURATION           (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "duration")),
    TIME               (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "time")),
    DATE               (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "date")),
    GYEARMONTH         (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "gYearMonth")),
    GYEAR              (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "gYear")),
    GMONTHDAY          (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "gMonthDay")),
    GDAY               (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "gDay")),
    GMONTH             (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "gMonth")),
    BOOLEAN            (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "boolean")),
    FLOAT              (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "float")),
    DECIMAL            (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "decimal")),
    DOUBLE             (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "double")),
    ANYURI             (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "anyURI")),
    QNAME              (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "QNAME")),
    NOTATION           (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "NOTATION")),
    BASE64             (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "base64Binary")),
    HEXBINARY          (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "hexBinary")),
    DATETIME           (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "dateTime")),
    INTEGER            (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "integer")),
    NONNEGATIVEINTEGER (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "nonNegativeInteger")),
    UNSIGNEDLONG       (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "unsignedLong")),
    POSITIVEINTEGER    (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "positiveInteger")),
    UNSIGNEDINT        (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "unsignedInt")),
    UNSIGNEDSHORT      (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "unsignedShort")),
    LONG               (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "long")),
    INT                (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "int")),
    SHORT              (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "short")),
    BYTE               (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "byte")),
    NONPOSITIVEINTEGER (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "nonPositiveInteger")),
    NEGATIVEINTEGER    (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "negativeInteger")),
    STRING             (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "string")),
    NORMALIZEDSTRING   (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "normalizedString")),
    TOKEN              (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "token")),
    LANGUAGE           (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "language")),
    NAME               (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "name")),
    NMTOKEN            (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "nmtoken")),
    NCNAME             (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "ncname")),
    NMTOKENS           (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "nmtokens")),
    ID                 (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "id")),
    IDREF              (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "idref")),
    ENTITY             (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "entity")),
    IDREFS             (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "idrefs")),
    ENTITIES           (new QName(XMLConstants.W3C_XML_SCHEMA_NS_URI, "entities")),
    ;
    
    private XmlSchemaDataTypes(QName qname) {
        this.qname = qname;
    }
    
    public static XmlSchemaDataTypes type(QName qname) {
        XmlSchemaDataTypes typeMatch = NOTYPE;
        for (XmlSchemaDataTypes type : types.keySet()) {
            if (type.qname.equals(qname)) {
                typeMatch = type;
            }
        }
        return typeMatch;
    }

    /**
     * Decides if the data type represented by this enumerated item is the
     * ancestor of the specified type.
     *
     * @param name Name of type that is potentially a direct or indirect
     * descendant of this item's type.
     *
     * @return true if the specified type name identifies a descendant of this
     *         item's type, false otherwise.
     */
    public boolean isDerivableTo(QName name) {
        boolean isDerivableTo = false;
        Collection<XmlSchemaDataTypes> derivations = types.get(this);

        if (derivations != null) {
            // Immediate derivatives
            for (Iterator<XmlSchemaDataTypes> iter = derivations.iterator();
                 iter.hasNext() && !isDerivableTo;) {
                isDerivableTo = iter.next().qname.equals(name);
            }

            // Indirect derivatives
            if (!isDerivableTo) {
                for (Iterator<XmlSchemaDataTypes> iter = derivations.iterator();
                     iter.hasNext() && !isDerivableTo;) {
                    isDerivableTo = iter.next().isDerivableTo(name);
                }
            }
        }

        return isDerivableTo;
    }

    /**
     * Produces the set of names of data types that identify this enumerated
     * item's data type and all of its known descendants.
     *
     * @return A collection of QName objects identifying this item's type and
     *         its known descedants'.
     */
    public Collection<QName> getLineage() {
        Collection<QName> lineage = new HashSet<QName>();
        lineage.add(qname);

        Collection<XmlSchemaDataTypes> derivations = types.get(this);
        if (derivations != null) {
            for (XmlSchemaDataTypes derivation : derivations) {
                lineage.add(derivation.qname);
            }
            for (XmlSchemaDataTypes derivation : derivations) {
                lineage.addAll(derivation.getLineage());
            }
        }
        return lineage;
    }

    private static final Map<XmlSchemaDataTypes, Collection<XmlSchemaDataTypes>>
            types;
    public final QName qname;
    
    static {
        // type hierarchy
        types = new HashMap<XmlSchemaDataTypes, Collection<XmlSchemaDataTypes>>();

        // anySimpleTypes sub hierarchy
        {
            types.put(DURATION, new HashSet<XmlSchemaDataTypes>());
            types.put(TIME, new HashSet<XmlSchemaDataTypes>());
            types.put(DATE, new HashSet<XmlSchemaDataTypes>());
            types.put(GYEARMONTH, new HashSet<XmlSchemaDataTypes>());
            types.put(GYEAR, new HashSet<XmlSchemaDataTypes>());
            types.put(GMONTHDAY, new HashSet<XmlSchemaDataTypes>());
            types.put(GDAY, new HashSet<XmlSchemaDataTypes>());
            types.put(GMONTH, new HashSet<XmlSchemaDataTypes>());
            types.put(BOOLEAN, new HashSet<XmlSchemaDataTypes>());
            types.put(BASE64, new HashSet<XmlSchemaDataTypes>());
            types.put(HEXBINARY, new HashSet<XmlSchemaDataTypes>());
            types.put(FLOAT, new HashSet<XmlSchemaDataTypes>());
            types.put(DOUBLE, new HashSet<XmlSchemaDataTypes>());
            types.put(ANYURI, new HashSet<XmlSchemaDataTypes>());
            types.put(QNAME, new HashSet<XmlSchemaDataTypes>());
            types.put(NOTATION, new HashSet<XmlSchemaDataTypes>());
            types.put(ANYSPECIALTYPE,
                    Arrays.asList(DURATION,
                            DATETIME,
                            TIME,
                            DATE,
                            GYEARMONTH,
                            GYEAR,
                            GMONTHDAY,
                            GDAY,
                            GMONTH,
                            BOOLEAN,
                            BASE64,
                            HEXBINARY,
                            FLOAT,
                            DECIMAL,
                            DOUBLE,
                            ANYURI,
                            QNAME,
                            NOTATION,
                            STRING
                    )
            );
        }
        
        // Decimal sub hierarchy
        {
            types.put(UNSIGNEDSHORT, new HashSet<XmlSchemaDataTypes>());
            types.put(UNSIGNEDINT, Arrays.asList(UNSIGNEDSHORT));
            types.put(UNSIGNEDLONG, Arrays.asList(UNSIGNEDINT));
            types.put(POSITIVEINTEGER, new HashSet<XmlSchemaDataTypes>());
            types.put(NONNEGATIVEINTEGER, Arrays.asList(UNSIGNEDLONG, POSITIVEINTEGER));
            types.put(BYTE, new HashSet<XmlSchemaDataTypes>());
            types.put(SHORT, Arrays.asList(BYTE));
            types.put(INT, Arrays.asList(SHORT));
            types.put(LONG, Arrays.asList(INT));
            types.put(NEGATIVEINTEGER, new HashSet<XmlSchemaDataTypes>());
            types.put(NONPOSITIVEINTEGER, Arrays.asList(NEGATIVEINTEGER));
            types.put(INTEGER, Arrays.asList(NONNEGATIVEINTEGER, LONG, NONPOSITIVEINTEGER));
            types.put(DECIMAL, Arrays.asList(INTEGER));
        }
        
        // String sub hierarchy
        {
            types.put(IDREFS, new HashSet<XmlSchemaDataTypes>());
            types.put(ENTITIES, new HashSet<XmlSchemaDataTypes>());
            types.put(ID, new HashSet<XmlSchemaDataTypes>());
            types.put(NMTOKEN, new HashSet<XmlSchemaDataTypes>());
            types.put(LANGUAGE, new HashSet<XmlSchemaDataTypes>());
            types.put(IDREF, Arrays.asList(IDREFS));
            types.put(ENTITY, Arrays.asList(ENTITIES));
            types.put(NCNAME, Arrays.asList(ID, IDREF, ENTITY));
            types.put(NAME, Arrays.asList(NCNAME));
            types.put(NMTOKEN, Arrays.asList(NMTOKENS));
            types.put(TOKEN, Arrays.asList(LANGUAGE, NAME, NMTOKEN));
            types.put(NORMALIZEDSTRING, Arrays.asList(TOKEN));
            types.put(STRING, Arrays.asList(NORMALIZEDSTRING));
        }
    }
}
