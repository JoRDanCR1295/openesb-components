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
 * @(#)Utility.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.bpel.core.test.xpath.dom;

import javax.xml.namespace.QName;

import org.apache.xmlbeans.SchemaField;
import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaParticle;
import org.apache.xmlbeans.SchemaType;
import org.apache.xmlbeans.SchemaTypeSystem;

public class Utility {
    
    public static SchemaField getGlobalElement2(SchemaTypeSystem schemaTypeSystem, QName qName) {
        SchemaGlobalElement[] globalElements = schemaTypeSystem.globalElements();
        for (int i = 0; i < globalElements.length; i++) {
            SchemaGlobalElement globalElement = globalElements[i];
            QName elementQName = globalElement.getName();
            if (areEqual(elementQName, qName)) {
                return globalElement;
            }            
        }
        return null;
    }
    
    public static SchemaField getSchemaType(SchemaType type, QName childQName) {
        SchemaParticle schemaParticle = type.getContentModel();
        SchemaField childElement = null;
        
        if (schemaParticle != null) {
            childElement = getParticleElement(schemaParticle, childQName);
        }
        return childElement;
    }    
    
    public static SchemaField getGlobalElement(SchemaTypeSystem schemaTypeSystem, QName qName) {
        return schemaTypeSystem.findElement(qName);
    }
    
    public static SchemaType getGlobalType(SchemaTypeSystem schemaTypeSystem, QName qName) {
        SchemaType[] schemaTypes = schemaTypeSystem.globalTypes();
        for (int i = 0; i < schemaTypes.length; i++) {
            SchemaType schemaType = schemaTypes[i];
            QName elementQName = schemaType.getName();
            if (areEqual(elementQName, qName)) {
                return schemaType;
            }            
        }
        return null;
    }
    
    
    public static SchemaField getSchemaType(SchemaField globalElement, QName childQName) {
        return getChildElement(globalElement, childQName, true);
    }
    
    private static SchemaField getChildElement(SchemaField element, QName childQName, boolean firstTime) {
        SchemaField childElement = null;
        
        if (!firstTime) {
            QName elementQName = element.getName();
            if (areEqual(elementQName, childQName)) {
                return element;
            }
        }
        
        if(firstTime && element.getType().getComponentType() == SchemaType.TYPE) {
            SchemaParticle schemaParticle = element.getType().getContentModel();
            if (schemaParticle != null) {
                childElement = getParticleElement(schemaParticle, childQName);
            }
        }
        return childElement;
    }
    
    private static SchemaField getParticleElement(SchemaParticle schemaParticle, QName childQName) {
        SchemaField childElement = null;
        
        if (schemaParticle.getParticleType() == SchemaParticle.ELEMENT) {
            childElement = getChildElement((SchemaField)schemaParticle, childQName, false);
        } else {
            SchemaParticle[] children = schemaParticle.getParticleChildren();
            for (int i = 0; i < children.length; i++) {
                SchemaParticle child = children[i];
                childElement = getParticleElement(child, childQName);
                if (childElement != null) {
                    break;
                }
            }
        }
        return childElement;
    } 
    
    private static boolean areEqual(QName qName1, QName qName2) {
        return equalStrings(qName1.getNamespaceURI(), qName2.getNamespaceURI())
        && equalStrings(qName1.getLocalPart(), qName2.getLocalPart());
    }
    
    private static boolean equalStrings(String s1, String s2) {
        if (s1 == null && s2 != null) {
            return false;
        }
        if (s1 != null && s2 == null) {
            return false;
        }

        if (s1 != null && !s1.trim().equals(s2.trim())) {
            return false;
        }
        return true;
    }
}
