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
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for DLD complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="DLD">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}DLD.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}DLD.2" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "DLD", propOrder = {
    "dld1",
    "dld2"
})
public class DLD {

    @XmlElement(name = "DLD.1")
    protected DLD1CONTENT dld1;
    @XmlElement(name = "DLD.2")
    protected DLD2CONTENT dld2;

    /**
     * Gets the value of the dld1 property.
     * 
     * @return
     *     possible object is
     *     {@link DLD1CONTENT }
     *     
     */
    public DLD1CONTENT getDLD1() {
        return dld1;
    }

    /**
     * Sets the value of the dld1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link DLD1CONTENT }
     *     
     */
    public void setDLD1(DLD1CONTENT value) {
        this.dld1 = value;
    }

    /**
     * Gets the value of the dld2 property.
     * 
     * @return
     *     possible object is
     *     {@link DLD2CONTENT }
     *     
     */
    public DLD2CONTENT getDLD2() {
        return dld2;
    }

    /**
     * Sets the value of the dld2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link DLD2CONTENT }
     *     
     */
    public void setDLD2(DLD2CONTENT value) {
        this.dld2 = value;
    }

}
