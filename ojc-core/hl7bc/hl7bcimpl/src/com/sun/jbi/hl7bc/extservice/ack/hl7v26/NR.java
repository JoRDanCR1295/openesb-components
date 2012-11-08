//
// This file was generated by the JavaTM Architecture for XML Binding(JAXB) Reference Implementation, v2.1-b02-fcs 
// See <a href="http://java.sun.com/xml/jaxb">http://java.sun.com/xml/jaxb</a> 
// Any modifications to this file will be lost upon recompilation of the source schema. 
// Generated on: 2008.12.04 at 12:45:30 AM IST 
//


package com.sun.jbi.hl7bc.extservice.ack.hl7v26;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for NR complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="NR">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}NR.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}NR.2" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "NR", propOrder = {
    "nr1",
    "nr2"
})
@XmlSeeAlso({
    RFR4CONTENT.class,
    CD6CONTENT.class,
    RFR1CONTENT.class,
    DLT1CONTENT.class,
    RFR3CONTENT.class
})
public class NR {

    @XmlElement(name = "NR.1")
    protected NR1CONTENT nr1;
    @XmlElement(name = "NR.2")
    protected NR2CONTENT nr2;

    /**
     * Gets the value of the nr1 property.
     * 
     * @return
     *     possible object is
     *     {@link NR1CONTENT }
     *     
     */
    public NR1CONTENT getNR1() {
        return nr1;
    }

    /**
     * Sets the value of the nr1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link NR1CONTENT }
     *     
     */
    public void setNR1(NR1CONTENT value) {
        this.nr1 = value;
    }

    /**
     * Gets the value of the nr2 property.
     * 
     * @return
     *     possible object is
     *     {@link NR2CONTENT }
     *     
     */
    public NR2CONTENT getNR2() {
        return nr2;
    }

    /**
     * Sets the value of the nr2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link NR2CONTENT }
     *     
     */
    public void setNR2(NR2CONTENT value) {
        this.nr2 = value;
    }

}
