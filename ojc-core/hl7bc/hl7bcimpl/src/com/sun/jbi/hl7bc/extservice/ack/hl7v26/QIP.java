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
 * <p>Java class for QIP complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="QIP">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element ref="{urn:hl7-org:v2xml}QIP.1" minOccurs="0"/>
 *         &lt;element ref="{urn:hl7-org:v2xml}QIP.2" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "QIP", propOrder = {
    "qip1",
    "qip2"
})
public class QIP {

    @XmlElement(name = "QIP.1")
    protected QIP1CONTENT qip1;
    @XmlElement(name = "QIP.2")
    protected QIP2CONTENT qip2;

    /**
     * Gets the value of the qip1 property.
     * 
     * @return
     *     possible object is
     *     {@link QIP1CONTENT }
     *     
     */
    public QIP1CONTENT getQIP1() {
        return qip1;
    }

    /**
     * Sets the value of the qip1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link QIP1CONTENT }
     *     
     */
    public void setQIP1(QIP1CONTENT value) {
        this.qip1 = value;
    }

    /**
     * Gets the value of the qip2 property.
     * 
     * @return
     *     possible object is
     *     {@link QIP2CONTENT }
     *     
     */
    public QIP2CONTENT getQIP2() {
        return qip2;
    }

    /**
     * Sets the value of the qip2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link QIP2CONTENT }
     *     
     */
    public void setQIP2(QIP2CONTENT value) {
        this.qip2 = value;
    }

}
