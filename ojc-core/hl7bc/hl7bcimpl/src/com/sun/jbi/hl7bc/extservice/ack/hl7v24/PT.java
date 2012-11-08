//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1-b02-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.04.18 at 09:20:26 PM IST 
//


package com.sun.jbi.hl7bc.extservice.ack.hl7v24;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for PT complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="PT">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}PT.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}PT.2" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "PT", namespace = "urn:hl7-org:v2xml", propOrder = {
    "pt1",
    "pt2"
})
@XmlSeeAlso({
    MSH11CONTENT.class
})
public class PT {

    @XmlElement(name = "PT.1", namespace = "urn:hl7-org:v2xml")
    protected PT1CONTENT pt1;
    @XmlElement(name = "PT.2", namespace = "urn:hl7-org:v2xml")
    protected PT2CONTENT pt2;

    /**
     * Gets the value of the pt1 property.
     * 
     * @return
     *     possible object is
     *     {@link PT1CONTENT }
     *     
     */
    public PT1CONTENT getPT1() {
        return pt1;
    }

    /**
     * Sets the value of the pt1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PT1CONTENT }
     *     
     */
    public void setPT1(PT1CONTENT value) {
        this.pt1 = value;
    }

    /**
     * Gets the value of the pt2 property.
     * 
     * @return
     *     possible object is
     *     {@link PT2CONTENT }
     *     
     */
    public PT2CONTENT getPT2() {
        return pt2;
    }

    /**
     * Sets the value of the pt2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link PT2CONTENT }
     *     
     */
    public void setPT2(PT2CONTENT value) {
        this.pt2 = value;
    }

}