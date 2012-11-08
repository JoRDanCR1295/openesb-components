//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1-b02-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2007.04.18 at 10:55:05 PM IST 
//


package com.sun.jbi.hl7bc.extservice.ack.hl7v23;

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
