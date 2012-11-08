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
 * @(#)SchemaInstanceParser.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**
 *
 */
package com.sun.jbi.cam.xml.configuration.schema;

import java.io.File;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaProperty;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeSystem;
import org.apache.xmlbeans.XmlAnySimpleType;
import org.apache.xmlbeans.XmlBeans;
import org.apache.xmlbeans.XmlDecimal;
import org.apache.xmlbeans.XmlException;
import org.apache.xmlbeans.XmlObject;
import org.apache.xmlbeans.XmlOptions;

/**
 * @author graj
 *
 */
public class SchemaInstanceParser implements Serializable {

    File[] schemaFiles;

    String[] schemaStrings;

    boolean allowNetworkDownloadsFlag;

    boolean disableParticleValidRestrictionFlag;

    boolean diableUniqueParticleAttributionFlag;

    SchemaTypeSystem schemaTypeSystem;

    SchemaType[] globalElements;

    /**
     *
     * @param schemaFilesObjects
     * @param allowNetworkDownloads
     * @param disableParticleValidRestriction
     * @param diableUniqueParticleAttribution
     */
    public SchemaInstanceParser(File[] schemaFilesObjects,
            boolean allowNetworkDownloads,
            boolean disableParticleValidRestriction,
            boolean diableUniqueParticleAttribution) {
        this.schemaFiles = schemaFilesObjects;
        this.allowNetworkDownloadsFlag = allowNetworkDownloads;
        this.disableParticleValidRestrictionFlag = disableParticleValidRestriction;
        this.diableUniqueParticleAttributionFlag = diableUniqueParticleAttribution;
        this.initialize();
    }

    /**
     *
     * @param schemaStringsObjects
     * @param allowNetworkDownloads
     * @param disableParticleValidRestriction
     * @param diableUniqueParticleAttribution
     */
    public SchemaInstanceParser(String[] schemaStringsObjects,
            boolean allowNetworkDownloads,
            boolean disableParticleValidRestriction,
            boolean diableUniqueParticleAttribution) {
        this.schemaStrings = schemaStringsObjects;
        this.allowNetworkDownloadsFlag = allowNetworkDownloads;
        this.disableParticleValidRestrictionFlag = disableParticleValidRestriction;
        this.diableUniqueParticleAttributionFlag = diableUniqueParticleAttribution;
        this.initialize();
    }

    /**
     *
     *
     */
    void initialize() {
        XmlObject[] schemas = null;
        if ((schemaFiles != null) && (schemaFiles.length > 0)) {
            schemas = processSchemaFiles(schemaFiles);
        } else if ((schemaStrings != null) && (schemaStrings.length > 0)) {
            schemas = processSchemaStrings(schemaStrings);
        } else {
            return;
        }

        if ((schemas != null) && (schemas.length > 0)) {
            this.schemaTypeSystem = compileSchema(schemas,
                    this.allowNetworkDownloadsFlag,
                    this.disableParticleValidRestrictionFlag,
                    this.diableUniqueParticleAttributionFlag);
        }
        if (schemaTypeSystem != null) {
            this.globalElements = this.schemaTypeSystem.documentTypes();
        }
    }

    /**
     *
     *
     * @param type
     * @return
     */
    public static String[] getEnumeratedStringValues(SchemaType type) {
        String[] returnValues = null;
        if (type.hasStringEnumValues() == true) {
            XmlAnySimpleType[] enumeratedValues = type.getEnumerationValues();
            if ((enumeratedValues != null) && (enumeratedValues.length > 0)) {
                returnValues = new String[enumeratedValues.length];
                for (int index = 0; index < enumeratedValues.length; index++) {
                    // System.out.println("--
                    // "+enumeratedValues[index].getStringValue());
                    returnValues[index] = enumeratedValues[index]
                            .getStringValue();
                }
            }
        }
        return returnValues;
    }

    /**
     *
     * @param elementName
     * @return
     */
    public SchemaType getGlobalSchemaTypeForName(String elementName) {
        if ((globalElements == null) || (globalElements.length <= 0)) {
            return null;
        }

        SchemaType element = null;
        for (int index = 0; index < globalElements.length; index++) {
            if (elementName.equals(globalElements[index]
                    .getDocumentElementName().getLocalPart())) {
                element = globalElements[index];
                break;
            }
        }

        if (element == null) {
            System.out.println("Could not find a global element with name \""
                    + elementName + "\"");
        }
        return element;
    }

    /**
     *
     * @param elementName
     * @param rootType
     * @return
     */
    public static SchemaType getCoreType(QName elementName, SchemaType rootType) {
        SchemaType returnValue = null;
        SchemaProperty[] schemaProperties = rootType.getElementProperties();
        if ((schemaProperties != null) && (schemaProperties.length > 0)) {
            for (int index = 0; index < schemaProperties.length; index++) {
                // System.out.println("Root's qName
                // "+schemaProperties[index].getName());
                // System.out.println("Root's type
                // "+schemaProperties[index].getType().getName());
                SchemaProperty[] innerProperties = schemaProperties[index]
                        .getType().getElementProperties();
                if ((innerProperties != null) && (innerProperties.length > 0)) {
                    for (int inner = 0; inner < innerProperties.length; inner++) {
                        // System.out.println("== Inner's qName
                        // "+innerProperties[inner].getName());
                        // System.out.println("== Inner's type
                        // "+innerProperties[inner].getType().getName());
                        if (elementName
                                .equals(innerProperties[inner].getName())) {
                            returnValue = findCoreType(innerProperties[inner]
                                    .getType());
                        }
                        // System.out.println("");
                    }
                }
            }
        }
        return returnValue;
    }

    /**
     *
     * @param elementType
     * @return
     */
    public static SchemaType findCoreType(SchemaType elementType) {
        SchemaProperty[] properties = elementType.getElementProperties();
        SchemaType type = null;
        if ((properties != null) && (properties.length > 0)) {
            for (int index = 0; index < properties.length; index++) {
                type = properties[index].getType();
                if (type != null) {
                    type = findCoreType(type);
                }
            }
        } else {
            type = elementType.getBaseType();
        }
        return type;

    }

    /**
     *
     * @param schemaFiles
     * @return
     */
    public static XmlObject[] processSchemaFiles(File[] schemaFileArray) {
        if ((schemaFileArray == null) || (schemaFileArray.length <= 0)) {
            return null;
        }
        List<XmlObject> schemaDocuments = new ArrayList<XmlObject>();
        for (int index = 0; index < schemaFileArray.length; index++) {
            try {
                schemaDocuments.add(XmlObject.Factory.parse(
                        schemaFileArray[index], (new XmlOptions())
                                .setLoadLineNumbers().setLoadMessageDigest()));
            } catch (Exception exception) {
                System.err.println("Can not load schema file: "
                        + schemaFileArray[index] + ": ");
                exception.printStackTrace();
            }
        }

        XmlObject[] schemas = (XmlObject[]) schemaDocuments
                .toArray(new XmlObject[schemaDocuments.size()]);

        return schemas;
    }

    /**
     *
     * @param schema strings
     * @return
     */
    public static XmlObject[] processSchemaStrings(String[] schemaFileArray) {
        if ((schemaFileArray == null) || (schemaFileArray.length <= 0)) {
            return null;
        }
        List<XmlObject> schemaDocuments = new ArrayList<XmlObject>();
        for (int index = 0; index < schemaFileArray.length; index++) {
            try {
                //System.out.println("Schema String:\n" + schemaFileArray[index]);
                schemaDocuments.add(XmlObject.Factory.parse(
                        schemaFileArray[index], (new XmlOptions())
                                .setLoadLineNumbers().setLoadMessageDigest()));
            } catch (Exception exception) {
                System.err.println("Can not load schema file: "
                        + schemaFileArray[index] + ": ");
                exception.printStackTrace();
            }
        }

        XmlObject[] schemas = (XmlObject[]) schemaDocuments
                .toArray(new XmlObject[schemaDocuments.size()]);

        return schemas;
    }

    /**
     *
     * @param schemas
     * @param allowNetworkDownloads
     * @param disableParticleValidRestriction
     * @param diableUniqueParticleAttribution
     * @return
     */
    public static SchemaTypeSystem compileSchema(XmlObject[] schemas,
            boolean allowNetworkDownloads,
            boolean disableParticleValidRestriction,
            boolean diableUniqueParticleAttribution) {
        if ((schemas == null) || (schemas.length <= 0)) {
            return null;
        }
        SchemaTypeSystem typeSystem = null;
        if (schemas.length > 0) {
            Collection errors = new ArrayList();
            XmlOptions compileOptions = new XmlOptions();
            if (true == allowNetworkDownloads) {
                compileOptions.setCompileDownloadUrls();
            }
            if (true == disableParticleValidRestriction) {
                compileOptions.setCompileNoPvrRule();
            }
            if (true == diableUniqueParticleAttribution) {
                compileOptions.setCompileNoUpaRule();
            }

            try {
                typeSystem = XmlBeans.compileXsd(schemas, XmlBeans
                        .getBuiltinTypeSystem(), compileOptions);
            } catch (Exception exception) {
                if (errors.isEmpty() || !(exception instanceof XmlException)) {
                    exception.printStackTrace();
                }

                System.out.println("Schema compilation errors: ");
                for (Iterator iterator = errors.iterator(); iterator.hasNext();) {
                    System.out.println(iterator.next());
                }
            }
        }

        if (typeSystem == null) {
            System.out.println("No Schemas to process.");
        }
        return typeSystem;
    }

    /**
     * @return the allowNetworkDownloadsFlag
     */
    public boolean isAllowNetworkDownloadsFlag() {
        return allowNetworkDownloadsFlag;
    }

    /**
     * @param allowNetworkDownloadsFlag
     *            the allowNetworkDownloadsFlag to set
     */
    public void setAllowNetworkDownloadsFlag(boolean allowNetworkDownloadsFlag) {
        this.allowNetworkDownloadsFlag = allowNetworkDownloadsFlag;
    }

    /**
     * @return the diableUniqueParticleAttributionFlag
     */
    public boolean isDiableUniqueParticleAttributionFlag() {
        return diableUniqueParticleAttributionFlag;
    }

    /**
     * @param diableUniqueParticleAttributionFlag
     *            the diableUniqueParticleAttributionFlag to set
     */
    public void setDiableUniqueParticleAttributionFlag(
            boolean diableUniqueParticleAttributionFlag) {
        this.diableUniqueParticleAttributionFlag = diableUniqueParticleAttributionFlag;
    }

    /**
     * @return the disableParticleValidRestrictionFlag
     */
    public boolean isDisableParticleValidRestrictionFlag() {
        return disableParticleValidRestrictionFlag;
    }

    /**
     * @param disableParticleValidRestrictionFlag
     *            the disableParticleValidRestrictionFlag to set
     */
    public void setDisableParticleValidRestrictionFlag(
            boolean disableParticleValidRestrictionFlag) {
        this.disableParticleValidRestrictionFlag = disableParticleValidRestrictionFlag;
    }

    /**
     * @return the globalElements
     */
    public SchemaType[] getGlobalElements() {
        return globalElements;
    }

    /**
     * @param globalElements
     *            the globalElements to set
     */
    public void setGlobalElements(SchemaType[] globalElements) {
        this.globalElements = globalElements;
    }

    /**
     * @return the schemaFiles
     */
    public File[] getSchemaFiles() {
        return schemaFiles;
    }

    /**
     * @param schemaFiles
     *            the schemaFiles to set
     */
    public void setSchemaFiles(File[] schemaFiles) {
        this.schemaFiles = schemaFiles;
    }

    /**
     * @return the schemaTypeSystem
     */
    public SchemaTypeSystem getSchemaTypeSystem() {
        return schemaTypeSystem;
    }

    /**
     * @param schemaTypeSystem
     *            the schemaTypeSystem to set
     */
    public void setSchemaTypeSystem(SchemaTypeSystem schemaTypeSystem) {
        this.schemaTypeSystem = schemaTypeSystem;
    }

    public static int getDecimalFacet(SchemaType elementType, int facetType) {
        int value = -1;
        XmlDecimal decimal = (XmlDecimal) elementType.getFacet(facetType);
        if (decimal != null) {
            value = decimal.getBigDecimalValue().intValue();
        }
        return value;
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        File[] schemaFiles = null;
        boolean allowNetworkDownloadsFlag = false;
        boolean disableParticleValidRestrictionFlag = false;
        boolean diableUniqueParticleAttributionFlag = false;
        String fileLocation = null;
        // System.getProperty("JBICOMPS_HOME") + "/cachese/jbiadapter/componentconfiguration.xsd";
        // System.getProperty("JBICOMPS_HOME") + "/cachese/jbiadapter/aspectmappolicy.xsd";
        // System.getProperty("JBICOMPS_HOME") + "/cachese/jbiadapter/serviceunitconfiguration.xsd";
        fileLocation = System.getProperty("JBICOMPS_HOME") + "/cachese/jbiadapter/componentconfiguration.xsd";
        File file = new File(fileLocation);
        schemaFiles = new File[1];
        schemaFiles[0] = file;

        SchemaInstanceParser parser = new SchemaInstanceParser(schemaFiles,
                allowNetworkDownloadsFlag, disableParticleValidRestrictionFlag,
                diableUniqueParticleAttributionFlag);
        SchemaType[] types = parser.getGlobalElements();
        if (types != null) {
            for (int index = 0; index < types.length; index++) {
                System.out.println("QName: "
                        + types[index].getDocumentElementName());

            }
        }

        QName qName = null;
        String prefix = types[0].getDocumentElementName().getPrefix();
        String namespaceURI = types[0].getDocumentElementName()
                .getNamespaceURI();

        SchemaType coreType = null;
        String[] enumValues = null;

        String[] attributeNames = { "CachingStrategy", "MaximumEntries" };
        //      String[] attributeNames = { "partnerLink", "roleName", "portType",
        //      "operation", "messageType", "file", "transformJBI", "input",
        //      "output", "requestReplyService", "filterOneWay",
        //      "filterRequestReply" };

        for (int count = 0; count < attributeNames.length; count++) {
            qName = new QName(namespaceURI, attributeNames[count], prefix);
            coreType = getCoreType(qName, types[0]);
            if (coreType != null) {
                System.out.println(attributeNames[count] + " Core Type is:"
                        + coreType.getName());
                System.out.println(attributeNames[count]
                        + " Core Base type is:"
                        + coreType.getBaseType().getName());
                enumValues = getEnumeratedStringValues(coreType);
                if ((enumValues != null) && (enumValues.length > 0)) {
                    for (int index = 0; index < enumValues.length; index++) {
                        System.out.println(enumValues[index]);
                    }
                }
                int totalDigits = 0, minInclusive = 0, maxInclusive = 0;
                totalDigits = getDecimalFacet(coreType,
                        SchemaType.FACET_TOTAL_DIGITS);
                minInclusive = getDecimalFacet(coreType,
                        SchemaType.FACET_MIN_INCLUSIVE);
                maxInclusive = getDecimalFacet(coreType,
                        SchemaType.FACET_MAX_INCLUSIVE);
                System.out.println("totalDigits=" + totalDigits
                        + ", minInclusive=" + minInclusive + ", maxInclusive="
                        + maxInclusive);
                System.out.println("////////////////////");
            }

        }
    }

}
