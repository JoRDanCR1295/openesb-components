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
 * @(#)ObjectFactory.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

/**************************************************************************
 *
 *          Copyright (c) 2005, Sun Microsystems,
 *          All Rights Reserved
 *
 *          This program, and all the routines referenced herein,
 *          are the proprietary properties and trade secrets of
 *          Sun Microsystems.
 *
 *          Except as provided for by license agreement, this
 *          program shall not be duplicated, used, or disclosed
 *          without  written consent signed by an officer of
 *          Sun Microsystems.
 *
 ***************************************************************************/


package com.sun.jbi.hl7bc.extservice.ack.hl7v21;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.ACKCONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.ERR1CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.ERRCONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSA1CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSA2CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSA3CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSA4CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSA5CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSACONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSH10CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSH11CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSH12CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSH13CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSH14CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSH1CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSH2CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSH3CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSH4CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSH5CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSH6CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSH7CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSH8CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSH9CONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.MSHCONTENT;
import com.sun.jbi.hl7bc.extservice.ack.hl7v21.ObjectFactory;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.sun.jbi.hl7bc.extservice.ack.hl7v21 package. 
 * <p>An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _MSH1_QNAME = new QName("urn:hl7-org:v2xml", "MSH.1");
    private final static QName _MSH11_QNAME = new QName("urn:hl7-org:v2xml", "MSH.11");
    private final static QName _MSA5_QNAME = new QName("urn:hl7-org:v2xml", "MSA.5");
    private final static QName _MSH2_QNAME = new QName("urn:hl7-org:v2xml", "MSH.2");
    private final static QName _MSA2_QNAME = new QName("urn:hl7-org:v2xml", "MSA.2");
    private final static QName _MSH9_QNAME = new QName("urn:hl7-org:v2xml", "MSH.9");
    private final static QName _MSH5_QNAME = new QName("urn:hl7-org:v2xml", "MSH.5");
    private final static QName _MSH14_QNAME = new QName("urn:hl7-org:v2xml", "MSH.14");
    private final static QName _MSH10_QNAME = new QName("urn:hl7-org:v2xml", "MSH.10");
    private final static QName _MSA4_QNAME = new QName("urn:hl7-org:v2xml", "MSA.4");
    private final static QName _MSH_QNAME = new QName("urn:hl7-org:v2xml", "MSH");
    private final static QName _MSH13_QNAME = new QName("urn:hl7-org:v2xml", "MSH.13");
    private final static QName _MSH7_QNAME = new QName("urn:hl7-org:v2xml", "MSH.7");
    private final static QName _ERR1_QNAME = new QName("urn:hl7-org:v2xml", "ERR.1");
    private final static QName _ACK_QNAME = new QName("urn:hl7-org:v2xml", "ACK");
    private final static QName _MSA3_QNAME = new QName("urn:hl7-org:v2xml", "MSA.3");
    private final static QName _MSH4_QNAME = new QName("urn:hl7-org:v2xml", "MSH.4");
    private final static QName _MSH6_QNAME = new QName("urn:hl7-org:v2xml", "MSH.6");
    private final static QName _MSA_QNAME = new QName("urn:hl7-org:v2xml", "MSA");
    private final static QName _MSA1_QNAME = new QName("urn:hl7-org:v2xml", "MSA.1");
    private final static QName _MSH12_QNAME = new QName("urn:hl7-org:v2xml", "MSH.12");
    private final static QName _MSH8_QNAME = new QName("urn:hl7-org:v2xml", "MSH.8");
    private final static QName _MSH3_QNAME = new QName("urn:hl7-org:v2xml", "MSH.3");
    private final static QName _ERR_QNAME = new QName("urn:hl7-org:v2xml", "ERR");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.sun.jbi.hl7bc.extservice.ack.hl7v21
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link MSH10CONTENT }
     * 
     */
    public MSH10CONTENT createMSH10CONTENT() {
        return new MSH10CONTENT();
    }

    /**
     * Create an instance of {@link MSH1CONTENT }
     * 
     */
    public MSH1CONTENT createMSH1CONTENT() {
        return new MSH1CONTENT();
    }

    /**
     * Create an instance of {@link MSA4CONTENT }
     * 
     */
    public MSA4CONTENT createMSA4CONTENT() {
        return new MSA4CONTENT();
    }

    /**
     * Create an instance of {@link MSH13CONTENT }
     * 
     */
    public MSH13CONTENT createMSH13CONTENT() {
        return new MSH13CONTENT();
    }

    /**
     * Create an instance of {@link MSH8CONTENT }
     * 
     */
    public MSH8CONTENT createMSH8CONTENT() {
        return new MSH8CONTENT();
    }

    /**
     * Create an instance of {@link MSHCONTENT }
     * 
     */
    public MSHCONTENT createMSHCONTENT() {
        return new MSHCONTENT();
    }

    /**
     * Create an instance of {@link MSH11CONTENT }
     * 
     */
    public MSH11CONTENT createMSH11CONTENT() {
        return new MSH11CONTENT();
    }

    /**
     * Create an instance of {@link MSH3CONTENT }
     * 
     */
    public MSH3CONTENT createMSH3CONTENT() {
        return new MSH3CONTENT();
    }

    /**
     * Create an instance of {@link ERRCONTENT }
     * 
     */
    public ERRCONTENT createERRCONTENT() {
        return new ERRCONTENT();
    }

    /**
     * Create an instance of {@link MSA2CONTENT }
     * 
     */
    public MSA2CONTENT createMSA2CONTENT() {
        return new MSA2CONTENT();
    }

    /**
     * Create an instance of {@link MSA5CONTENT }
     * 
     */
    public MSA5CONTENT createMSA5CONTENT() {
        return new MSA5CONTENT();
    }

    /**
     * Create an instance of {@link MSA3CONTENT }
     * 
     */
    public MSA3CONTENT createMSA3CONTENT() {
        return new MSA3CONTENT();
    }

    /**
     * Create an instance of {@link MSH6CONTENT }
     * 
     */
    public MSH6CONTENT createMSH6CONTENT() {
        return new MSH6CONTENT();
    }

    /**
     * Create an instance of {@link MSH7CONTENT }
     * 
     */
    public MSH7CONTENT createMSH7CONTENT() {
        return new MSH7CONTENT();
    }

    /**
     * Create an instance of {@link MSA1CONTENT }
     * 
     */
    public MSA1CONTENT createMSA1CONTENT() {
        return new MSA1CONTENT();
    }

    /**
     * Create an instance of {@link MSH2CONTENT }
     * 
     */
    public MSH2CONTENT createMSH2CONTENT() {
        return new MSH2CONTENT();
    }

    /**
     * Create an instance of {@link MSH14CONTENT }
     * 
     */
    public MSH14CONTENT createMSH14CONTENT() {
        return new MSH14CONTENT();
    }

    /**
     * Create an instance of {@link ERR1CONTENT }
     * 
     */
    public ERR1CONTENT createERR1CONTENT() {
        return new ERR1CONTENT();
    }

    /**
     * Create an instance of {@link MSH4CONTENT }
     * 
     */
    public MSH4CONTENT createMSH4CONTENT() {
        return new MSH4CONTENT();
    }

    /**
     * Create an instance of {@link MSH9CONTENT }
     * 
     */
    public MSH9CONTENT createMSH9CONTENT() {
        return new MSH9CONTENT();
    }

    /**
     * Create an instance of {@link ACKCONTENT }
     * 
     */
    public ACKCONTENT createACKCONTENT() {
        return new ACKCONTENT();
    }

    /**
     * Create an instance of {@link MSACONTENT }
     * 
     */
    public MSACONTENT createMSACONTENT() {
        return new MSACONTENT();
    }

    /**
     * Create an instance of {@link MSH12CONTENT }
     * 
     */
    public MSH12CONTENT createMSH12CONTENT() {
        return new MSH12CONTENT();
    }

    /**
     * Create an instance of {@link MSH5CONTENT }
     * 
     */
    public MSH5CONTENT createMSH5CONTENT() {
        return new MSH5CONTENT();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSH1CONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSH.1")
    public JAXBElement<MSH1CONTENT> createMSH1(MSH1CONTENT value) {
        return new JAXBElement<MSH1CONTENT>(_MSH1_QNAME, MSH1CONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSH11CONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSH.11")
    public JAXBElement<MSH11CONTENT> createMSH11(MSH11CONTENT value) {
        return new JAXBElement<MSH11CONTENT>(_MSH11_QNAME, MSH11CONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSA5CONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSA.5")
    public JAXBElement<MSA5CONTENT> createMSA5(MSA5CONTENT value) {
        return new JAXBElement<MSA5CONTENT>(_MSA5_QNAME, MSA5CONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSH2CONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSH.2")
    public JAXBElement<MSH2CONTENT> createMSH2(MSH2CONTENT value) {
        return new JAXBElement<MSH2CONTENT>(_MSH2_QNAME, MSH2CONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSA2CONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSA.2")
    public JAXBElement<MSA2CONTENT> createMSA2(MSA2CONTENT value) {
        return new JAXBElement<MSA2CONTENT>(_MSA2_QNAME, MSA2CONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSH9CONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSH.9")
    public JAXBElement<MSH9CONTENT> createMSH9(MSH9CONTENT value) {
        return new JAXBElement<MSH9CONTENT>(_MSH9_QNAME, MSH9CONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSH5CONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSH.5")
    public JAXBElement<MSH5CONTENT> createMSH5(MSH5CONTENT value) {
        return new JAXBElement<MSH5CONTENT>(_MSH5_QNAME, MSH5CONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSH14CONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSH.14")
    public JAXBElement<MSH14CONTENT> createMSH14(MSH14CONTENT value) {
        return new JAXBElement<MSH14CONTENT>(_MSH14_QNAME, MSH14CONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSH10CONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSH.10")
    public JAXBElement<MSH10CONTENT> createMSH10(MSH10CONTENT value) {
        return new JAXBElement<MSH10CONTENT>(_MSH10_QNAME, MSH10CONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSA4CONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSA.4")
    public JAXBElement<MSA4CONTENT> createMSA4(MSA4CONTENT value) {
        return new JAXBElement<MSA4CONTENT>(_MSA4_QNAME, MSA4CONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSHCONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSH")
    public JAXBElement<MSHCONTENT> createMSH(MSHCONTENT value) {
        return new JAXBElement<MSHCONTENT>(_MSH_QNAME, MSHCONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSH13CONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSH.13")
    public JAXBElement<MSH13CONTENT> createMSH13(MSH13CONTENT value) {
        return new JAXBElement<MSH13CONTENT>(_MSH13_QNAME, MSH13CONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSH7CONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSH.7")
    public JAXBElement<MSH7CONTENT> createMSH7(MSH7CONTENT value) {
        return new JAXBElement<MSH7CONTENT>(_MSH7_QNAME, MSH7CONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ERR1CONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "ERR.1")
    public JAXBElement<ERR1CONTENT> createERR1(ERR1CONTENT value) {
        return new JAXBElement<ERR1CONTENT>(_ERR1_QNAME, ERR1CONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ACKCONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "ACK")
    public JAXBElement<ACKCONTENT> createACK(ACKCONTENT value) {
        return new JAXBElement<ACKCONTENT>(_ACK_QNAME, ACKCONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSA3CONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSA.3")
    public JAXBElement<MSA3CONTENT> createMSA3(MSA3CONTENT value) {
        return new JAXBElement<MSA3CONTENT>(_MSA3_QNAME, MSA3CONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSH4CONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSH.4")
    public JAXBElement<MSH4CONTENT> createMSH4(MSH4CONTENT value) {
        return new JAXBElement<MSH4CONTENT>(_MSH4_QNAME, MSH4CONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSH6CONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSH.6")
    public JAXBElement<MSH6CONTENT> createMSH6(MSH6CONTENT value) {
        return new JAXBElement<MSH6CONTENT>(_MSH6_QNAME, MSH6CONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSACONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSA")
    public JAXBElement<MSACONTENT> createMSA(MSACONTENT value) {
        return new JAXBElement<MSACONTENT>(_MSA_QNAME, MSACONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSA1CONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSA.1")
    public JAXBElement<MSA1CONTENT> createMSA1(MSA1CONTENT value) {
        return new JAXBElement<MSA1CONTENT>(_MSA1_QNAME, MSA1CONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSH12CONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSH.12")
    public JAXBElement<MSH12CONTENT> createMSH12(MSH12CONTENT value) {
        return new JAXBElement<MSH12CONTENT>(_MSH12_QNAME, MSH12CONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSH8CONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSH.8")
    public JAXBElement<MSH8CONTENT> createMSH8(MSH8CONTENT value) {
        return new JAXBElement<MSH8CONTENT>(_MSH8_QNAME, MSH8CONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link MSH3CONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "MSH.3")
    public JAXBElement<MSH3CONTENT> createMSH3(MSH3CONTENT value) {
        return new JAXBElement<MSH3CONTENT>(_MSH3_QNAME, MSH3CONTENT.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link ERRCONTENT }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "urn:hl7-org:v2xml", name = "ERR")
    public JAXBElement<ERRCONTENT> createERR(ERRCONTENT value) {
        return new JAXBElement<ERRCONTENT>(_ERR_QNAME, ERRCONTENT.class, null, value);
    }

}
