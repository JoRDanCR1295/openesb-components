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
 * @(#)TS.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.ack.hl7v251;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for TS complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="TS">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}TS.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}TS.2" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "TS", namespace = "urn:hl7-org:v2xml", propOrder = {
    "ts1",
    "ts2"
})
@XmlSeeAlso({
    SFT6CONTENT.class,
    MSH7CONTENT.class
})
public class TS {

    @XmlElement(name = "TS.1", namespace = "urn:hl7-org:v2xml")
    protected TS1CONTENT ts1;
    @XmlElement(name = "TS.2", namespace = "urn:hl7-org:v2xml")
    protected TS2CONTENT ts2;

    /**
     * Gets the value of the ts1 property.
     * 
     * @return
     *     possible object is
     *     {@link TS1CONTENT }
     *     
     */
    public TS1CONTENT getTS1() {
        return ts1;
    }

    /**
     * Sets the value of the ts1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link TS1CONTENT }
     *     
     */
    public void setTS1(TS1CONTENT value) {
        this.ts1 = value;
    }

    /**
     * Gets the value of the ts2 property.
     * 
     * @return
     *     possible object is
     *     {@link TS2CONTENT }
     *     
     */
    public TS2CONTENT getTS2() {
        return ts2;
    }

    /**
     * Sets the value of the ts2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link TS2CONTENT }
     *     
     */
    public void setTS2(TS2CONTENT value) {
        this.ts2 = value;
    }

}
