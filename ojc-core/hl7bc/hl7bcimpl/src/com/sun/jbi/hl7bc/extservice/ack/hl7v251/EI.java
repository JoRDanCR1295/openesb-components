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
 * @(#)EI.java 
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
 * <p>Java class for EI complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="EI">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}EI.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}EI.2" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}EI.3" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}EI.4" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "EI", namespace = "urn:hl7-org:v2xml", propOrder = {
    "ei1",
    "ei2",
    "ei3",
    "ei4"
})
@XmlSeeAlso({
    MSH21CONTENT.class
})
public class EI {

    @XmlElement(name = "EI.1", namespace = "urn:hl7-org:v2xml")
    protected EI1CONTENT ei1;
    @XmlElement(name = "EI.2", namespace = "urn:hl7-org:v2xml")
    protected EI2CONTENT ei2;
    @XmlElement(name = "EI.3", namespace = "urn:hl7-org:v2xml")
    protected EI3CONTENT ei3;
    @XmlElement(name = "EI.4", namespace = "urn:hl7-org:v2xml")
    protected EI4CONTENT ei4;

    /**
     * Gets the value of the ei1 property.
     * 
     * @return
     *     possible object is
     *     {@link EI1CONTENT }
     *     
     */
    public EI1CONTENT getEI1() {
        return ei1;
    }

    /**
     * Sets the value of the ei1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link EI1CONTENT }
     *     
     */
    public void setEI1(EI1CONTENT value) {
        this.ei1 = value;
    }

    /**
     * Gets the value of the ei2 property.
     * 
     * @return
     *     possible object is
     *     {@link EI2CONTENT }
     *     
     */
    public EI2CONTENT getEI2() {
        return ei2;
    }

    /**
     * Sets the value of the ei2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link EI2CONTENT }
     *     
     */
    public void setEI2(EI2CONTENT value) {
        this.ei2 = value;
    }

    /**
     * Gets the value of the ei3 property.
     * 
     * @return
     *     possible object is
     *     {@link EI3CONTENT }
     *     
     */
    public EI3CONTENT getEI3() {
        return ei3;
    }

    /**
     * Sets the value of the ei3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link EI3CONTENT }
     *     
     */
    public void setEI3(EI3CONTENT value) {
        this.ei3 = value;
    }

    /**
     * Gets the value of the ei4 property.
     * 
     * @return
     *     possible object is
     *     {@link EI4CONTENT }
     *     
     */
    public EI4CONTENT getEI4() {
        return ei4;
    }

    /**
     * Sets the value of the ei4 property.
     * 
     * @param value
     *     allowed object is
     *     {@link EI4CONTENT }
     *     
     */
    public void setEI4(EI4CONTENT value) {
        this.ei4 = value;
    }

}
