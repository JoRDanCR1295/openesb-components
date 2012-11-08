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
 * @(#)MSHInfo.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.hl7bc.extservice.ack;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.Unmarshaller;

import org.w3c.dom.Node;

import com.sun.jbi.hl7bc.extservice.ack.hl7v25.*;
import com.sun.jbi.hl7bc.HL7Constants;

/**
 * Wrapper around MSH segment
 * 
 * @author S. Nageswara Rao
 */

public class MSHInfo implements HL7Constants {

   // private static MSHInfo singleton = null;

    private static JAXBContext jaxbContext = null;

    private Unmarshaller unmarshaller = null;

    private Node mMSHSeg;

    private MSHCONTENT mMSHContent;

  /*  static {
        try {
            // acclerate the JAXBContext Initialization
            System.setProperty("com.sun.xml.bind.v2.runtime.JAXBContextImpl.fastBoot", "true");
            // create a JAXBContext capable of handling class MSHCONTENT.class
            jaxbContext = JAXBContext.newInstance(MSHCONTENT.class);
            // create an Unmarshaller
            unmarshaller = jaxbContext.createUnmarshaller();
        } catch (Exception exc) {
            exc.printStackTrace();
        }
    }*/
    public MSHInfo() {
        try {
            // acclerate the JAXBContext Initialization
            System.setProperty("com.sun.xml.bind.v2.runtime.JAXBContextImpl.fastBoot", "false");
            // create a JAXBContext capable of handling class MSACONTENT.class
            if (jaxbContext == null) {
                jaxbContext = JAXBContext.newInstance(MSHCONTENT.class);
            }
            // create an Unmarshaller
            unmarshaller = jaxbContext.createUnmarshaller();
        } catch (Exception exc) {
            exc.printStackTrace();
        }


    }

   /* public static MSHInfo getInstance() {
        if (singleton == null) {
            singleton = new MSHInfo();
        }
        return singleton;
    }*/

    public void setMSHSegment(Node node) {
        this.mMSHSeg = node;
    }

    /**
     * Unmarshal the Dom Message to Java Bean
     * 
     * @throws Exception
     */
    public void unmarshal() throws Exception {
        // unmarshal MSH instance document into a tree of Java content
        // objects composed of classes from the com.sun.jbi.hl7bc.extservice.ack.hl7v231 package.
        JAXBElement<?> mshElement = (JAXBElement<?>) unmarshaller.unmarshal(mMSHSeg);
        mMSHContent = ((MSHCONTENT) mshElement.getValue());
    }

    /**
     * Returns the Sending Facility value
     * 
     * @return String
     */
    public String getSendingFacility() {
        char compSep = mMSHContent.getMSH2().getValue().charAt(0);
        StringBuilder sb = new StringBuilder();
        HD1CONTENT hd1 = mMSHContent.getMSH6().getHD1();
        HD2CONTENT hd2 = mMSHContent.getMSH6().getHD2();
        HD3CONTENT hd3 = mMSHContent.getMSH6().getHD3();
        if (hd1 != null)
            sb.append(hd1.getValue());
        if (hd2 != null)
            sb.append(compSep).append(hd2.getValue());
        if (hd3 != null)
            sb.append(compSep).append(hd3.getValue());
        return sb.toString();
    }

    /**
     * Returns the Sending Application field value
     * 
     * @return String
     */
    public String getSendingApplication() {
        char compSep = mMSHContent.getMSH2().getValue().charAt(0);
        StringBuilder sb = new StringBuilder();
        HD1CONTENT hd1 = mMSHContent.getMSH5().getHD1();
        HD2CONTENT hd2 = mMSHContent.getMSH5().getHD2();
        HD3CONTENT hd3 = mMSHContent.getMSH5().getHD3();
        if (hd1 != null)
            sb.append(hd1.getValue());
        if (hd2 != null)
            sb.append(compSep).append(hd2.getValue());
        if (hd3 != null)
            sb.append(compSep).append(hd3.getValue());
        return sb.toString();
    }

    /**
     * Returns the Receiving Facility field value
     * 
     * @return String
     */
    public String getReceivingFacility() {
        char compSep = mMSHContent.getMSH2().getValue().charAt(0);
        StringBuilder sb = new StringBuilder();
        HD1CONTENT hd1 = mMSHContent.getMSH4().getHD1();
        HD2CONTENT hd2 = mMSHContent.getMSH4().getHD2();
        HD3CONTENT hd3 = mMSHContent.getMSH4().getHD3();
        if (hd1 != null)
            sb.append(hd1.getValue());
        if (hd2 != null)
            sb.append(compSep).append(hd2.getValue());
        if (hd3 != null)
            sb.append(compSep).append(hd3.getValue());
        return sb.toString();
    }

    /**
     * Returns the Receiving Application field value
     * 
     * @return String
     */
    public String getReceivingApplication() {
        char compSep = mMSHContent.getMSH2().getValue().charAt(0);
        StringBuilder sb = new StringBuilder();
        HD1CONTENT hd1 = mMSHContent.getMSH3().getHD1();
        HD2CONTENT hd2 = mMSHContent.getMSH3().getHD2();
        HD3CONTENT hd3 = mMSHContent.getMSH3().getHD3();
        if (hd1 != null)
            sb.append(hd1.getValue());
        if (hd2 != null)
            sb.append(compSep).append(hd2.getValue());
        if (hd3 != null)
            sb.append(compSep).append(hd3.getValue());
        return sb.toString();
    }

    /**
     * Returs the Message Control ID field value
     * 
     * @return String
     */
    public String getMsgControlID() {
        return mMSHContent.getMSH10().getValue();
    }

    /**
     * Returs the Message Sequence Number field value
     * 
     * @return String
     */
    public int getSequenceNumber() throws Exception {
        int expSeqNo = INVALID_SEQNO;
        String mshSeqno = mMSHContent.getMSH13().getValue();
        if (mshSeqno == null || mshSeqno.equals("")) {
            return expSeqNo;
        }
        try {
            expSeqNo = Integer.parseInt(mshSeqno);
        } catch (Exception e) {
            return expSeqNo;
        }
        return expSeqNo;
    }
}
