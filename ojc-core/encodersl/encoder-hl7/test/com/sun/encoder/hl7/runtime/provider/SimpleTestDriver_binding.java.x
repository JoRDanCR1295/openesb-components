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
 * @(#)SimpleTestDriver.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.encoder.hl7.runtime.provider;

//import hl7OrgV2Xml.ADTA01Document;
//import hl7OrgV2Xml.DG110CONTENT;
//import hl7OrgV2Xml.DG1CONTENT;
//import hl7OrgV2Xml.GT1CONTENT;
//import hl7OrgV2Xml.IN1CONTENT;
//import hl7OrgV2Xml.NK1CONTENT;
//import hl7OrgV2Xml.OBXCONTENT;
//import hl7OrgV2Xml.PV1CONTENT;
//import hl7OrgV2Xml.PV2CONTENT;
//import hl7OrgV2Xml.ROLCONTENT;

import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBElement;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.bind.UnmarshallerHandler;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.stream.XMLEventReader;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.sax.SAXResult;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import javolution.text.CharArray;
import javolution.util.FastMap;
import javolution.xml.stream.XMLStreamReader;

import org.apache.xmlbeans.SchemaGlobalElement;
import org.apache.xmlbeans.SchemaTypeLoader;
import org.apache.xmlbeans.SchemaTypeSystem;
import org.apache.xmlbeans.XmlBeans;
import org.apache.xmlbeans.XmlCursor;
import org.apache.xmlbeans.XmlException;
import org.apache.xmlbeans.XmlObject;
import org.apache.xmlbeans.XmlOptions;
import org.apache.xmlbeans.XmlSaxHandler;
import org.hl7.v2xml.ADTA01CONTENT;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import com.stc.SeeBeyond.OTD_Library.HL7.X_2_5.HL7_25_ADT_A01.ADT_A01;
import com.stc.SeeBeyond.OTD_Library.HL7.X_2_5.HL7_25_DG1.Env1.DG1;
import com.stc.SeeBeyond.OTD_Library.HL7.X_2_5.HL7_25_EVN.Env1.EVN;
import com.stc.SeeBeyond.OTD_Library.HL7.X_2_5.HL7_25_GT1.Env1.GT1;
import com.stc.SeeBeyond.OTD_Library.HL7.X_2_5.HL7_25_IN1.Env1.IN1;
import com.stc.SeeBeyond.OTD_Library.HL7.X_2_5.HL7_25_MSH.Env1.MSH;
import com.stc.SeeBeyond.OTD_Library.HL7.X_2_5.HL7_25_NK1.Env1.NK1;
import com.stc.SeeBeyond.OTD_Library.HL7.X_2_5.HL7_25_OBX.Env1.OBX;
import com.stc.SeeBeyond.OTD_Library.HL7.X_2_5.HL7_25_PID.Env1.PID;
import com.stc.SeeBeyond.OTD_Library.HL7.X_2_5.HL7_25_PV1.Env1.PV1;
import com.stc.SeeBeyond.OTD_Library.HL7.X_2_5.HL7_25_PV2.Env1.PV2;
import com.stc.SeeBeyond.OTD_Library.HL7.X_2_5.HL7_25_ROL.Env1.ROL;
import com.sun.encoder.hl7.runtime.LexicalException;
import com.sun.encoder.hl7.runtime.provider.UnmarshalAdaptor.Lexer;
import com.sun.encoder.hl7.runtime.provider.UnmarshalAdaptor.Token;
import com.sun.encoder.util.UnicodeFile;
import com.ximpleware.EOFException;
import com.ximpleware.EncodingException;
import com.ximpleware.EntityException;
import com.ximpleware.ModifyException;
import com.ximpleware.NavException;
import com.ximpleware.ParseException;
import com.ximpleware.TranscodeException;
import com.ximpleware.VTDGen;
import com.ximpleware.VTDNav;
import com.ximpleware.XMLModifier;

/**
 * This class facilitates simple functional and performance testing on HL7
 * encoder.
 * 
 * @author Jun Xu
 */
public class SimpleTestDriver_binding {
    
    //define delimiter type names
    private static final Map<Integer, String> mDelimTypes =
        new HashMap<Integer, String>();
    static {
        mDelimTypes.put(
                UnmarshalAdaptor.Token.DELIM_NOT_READ,
                "delimiter.not.read");
        mDelimTypes.put(
                UnmarshalAdaptor.Token.SEG_TERM,
                "segment.terminator");
        mDelimTypes.put(
                UnmarshalAdaptor.Token.FIELD_SEP,
                "field.separator");
        mDelimTypes.put(
                UnmarshalAdaptor.Token.REPET_SEP,
                "repetition.separator");
        mDelimTypes.put(
                UnmarshalAdaptor.Token.COMPO_SEP,
                "component.separator");
        mDelimTypes.put(
                UnmarshalAdaptor.Token.SUBCOMPO_SEP,
                "subcomponent.separator");
    }
    
    //define token type names
    private static final Map<Integer, String> mTokenTypes =
        new HashMap<Integer, String>();
    static {
        mTokenTypes.put(UnmarshalAdaptor.Token.SEG_NAME, "segment.name");
        mTokenTypes.put(UnmarshalAdaptor.Token.VALUE, "value");
        mTokenTypes.put(UnmarshalAdaptor.Token.EOF, "end.of.file");
    }
    
    //prepare data for ADT_A44:
    private static final String mStringData;
    private static final byte[] mBytesData;
    static {
        StringBuffer sb = new StringBuffer();
        sb.append("MSH|^~\\&|REGADT|MCM|RSP1P8|MCM|").append(
                "199601051530|SEC|ADT^A44|00000007|P|2.3.1|||||").append(
                        "|UTF-8~ISO-8859-1~ISO-8859-2|||c1^sc1&c2\r");
        sb.append("EVN|A44|199601051530\r");
        sb.append("PID|||MR2^^^XYZ^MR||JONES^WILLIAM^A^JR||").append(
                "19501010|M|||123 EAST STREET^^NY^NY^10021|").append(
                        "|(212)111-3333|||S|ACCT|1\r");
        sb.append("MRG|MR1^^^XYZ^MR||ACCT1\r");
        sb.append("Z01|A44|199601051530");
        mStringData = sb.toString();
        try {
            mBytesData = mStringData.getBytes("UTF-8");
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
    }
    
    private final SchemaGlobalElement mRootElement;
    private final URL mSchemaLocation;
    
    public SimpleTestDriver_binding(URL schemaLocation, SchemaGlobalElement rootElement) {
        mRootElement = rootElement;
        mSchemaLocation = schemaLocation;
    }
    
    public void testLexingFromString(String stringData,
        int loops, boolean doPrint)
            throws IOException, LexicalException {
        System.out.println("data size (chars) = " + stringData.length());
        System.out.println("Start ...");
        long startedAt = System.currentTimeMillis();
        final int finalLoops = loops;
        final boolean finalDoPrint = doPrint;
        int i;
        for (i = 0; i < finalLoops; i++) {
            //Prepare lexer using StringReader
            Lexer lexer =
                new Lexer(
                    new InputSource(
                        new StringReader(stringData)),
                            stringData.length() + 1);
            //Do lexing
            Token token = new Token();
            while(lexer.fillNextToken(token)) {
                if (finalDoPrint) {
                    System.out.println(
                        new String(token.mChars, token.mOffset, token.mCount)
                            + ", " + token.mLine + ", " + token.mCol
                            + ", " + token.mLexerPos + ", "
                            + mTokenTypes.get(token.mTokenType) + ", "
                            + mDelimTypes.get(token.mDelimType));
                }
            }
        }
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }
        System.out.println("Characters/ms: "
                + i * stringData.length() / elapsed);
    }
    
    public void testLexingFromBytes(byte[] bytes, int loops, boolean doPrint)
            throws IOException, LexicalException {
        System.out.println("data size (bytes) = " + mStringData.length());
        System.out.println("Start ...");
        long startedAt = System.currentTimeMillis();
        final int finalLoops = loops;
        final boolean finalDoPrint = doPrint;
        int i;
        for (i = 0; i < finalLoops; i++) {
            //Prepare lexer using StringReader
            InputSource input = new InputSource(
                    new ByteArrayInputStream(bytes));
            input.setEncoding("UTF-8");
            Lexer lexer =
                new Lexer(input, bytes.length + 1);
            
            //Do lexing
            Token token = new Token();
            while(lexer.fillNextToken(token)) {
                if (finalDoPrint) {
                    System.out.println(
                        new String(token.mChars, token.mOffset, token.mCount)
                            + ", " + token.mLine + ", " + token.mCol
                            + ", " + token.mLexerPos + ", "
                            + mTokenTypes.get(token.mTokenType) + ", "
                            + mDelimTypes.get(token.mDelimType));
                }
            }
        }
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }
        System.out.println("Bytes/ms: "
                + i * bytes.length / elapsed);
    }

    public void testDecodeFromString(String stringData, int loops, boolean doPrint)
            throws XmlException, IOException, SAXException {
        System.out.println("data size (chars) = " + stringData.length());
        
        //Prepare content handler
        ContentHandler handler =
            new SimpleContentHandler(doPrint);
        
        System.out.println("Start ...");
        long startedAt = System.currentTimeMillis();
        final int finalLoops = loops;
        int i;
        UnmarshalAdaptor adaptor;
        for (i = 0; i < finalLoops; i++) {
            //Prepare UnmarshalAdaptor
            adaptor =
                new UnmarshalAdaptor(mSchemaLocation, mRootElement, stringData.length() + 1);
            adaptor.setContentHandler(handler);

            //Prepare InputSource
            InputSource input = new InputSource(new StringReader(stringData));
            input.setPublicId("string data");
            
            //Do parsing using simple content handler
            adaptor.parse(input);
        }
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }
        System.out.println("Characters/ms: "
                + i * stringData.length() / elapsed);
    }
    
    public void testDecodeIntoXmlBeansFromString(String stringData, int loops,
            boolean doPrint)
        throws XmlException, IOException, SAXException {
//        System.out.println("data size (chars) = " + stringData.length());
//        
//        //Prepare content handler
//        XmlSaxHandler handler = XmlObject.Factory.newXmlSaxHandler();
//        
//        System.out.println("Start ...");
//        final int finalLoops = loops;
//        int i;
//        UnmarshalAdaptor adaptor;
//        String val = null;
//        Runtime r = Runtime.getRuntime();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        long mem1 = Runtime.getRuntime().totalMemory()
//                        - Runtime.getRuntime().freeMemory();
//        ADTA01Document doc = null;
//        long startedAt = System.currentTimeMillis();
//        for (i = 0; i < finalLoops; i++) {
//            //Prepare UnmarshalAdaptor
//            adaptor =
//                new UnmarshalAdaptor(mSchemaLocation, mRootElement, 2048);
//            adaptor.setContentHandler(handler.getContentHandler());
//        
//            //Prepare InputSource
//            InputSource input = new InputSource(new StringReader(stringData));
//            input.setPublicId("string data");
//            
//            //Do parsing using simple content handler
//            adaptor.parse(input);
//            
//            doc = (ADTA01Document) handler.getObject();
//            hl7OrgV2Xml.ADTA01CONTENT adtA01 = doc.getADTA01();
//            //MSH
//            hl7OrgV2Xml.MSHCONTENT msh = adtA01.getMSH();
//            val = msh.getMSH1().getStringValue();
//            val = msh.getMSH2().getStringValue();
//            val = msh.getMSH3().getHD1().getStringValue();
//            val = msh.getMSH4().getHD1().getStringValue();
//            val = msh.getMSH5().getHD1().getStringValue();
//            val = msh.getMSH7().getTS1().getStringValue();
//            val = msh.getMSH9().getMSG1().getStringValue();
//            val = msh.getMSH9().getMSG2().getStringValue();
//            val = msh.getMSH9().getMSG3().getStringValue();
//            val = msh.getMSH10().getStringValue();
//            val = msh.getMSH11().getPT1().getStringValue();
//            val = msh.getMSH12().getVID1().getStringValue();
//            //EVN
//            hl7OrgV2Xml.EVNCONTENT evn = adtA01.getEVN();
//            val = evn.getEVN1().getStringValue();
//            val = evn.getEVN2().getTS1().getStringValue();
//            val = evn.getEVN3().getTS1().getStringValue();
//            val = evn.getEVN4().getStringValue();
//            val = evn.getEVN6().getTS1().getStringValue();
//            //PID
//            hl7OrgV2Xml.PIDCONTENT pid = adtA01.getPID();
//            val = pid.getPID3Array(0).getCX1().getStringValue();
//            val = pid.getPID3Array(0).getCX3().getStringValue();
//            val = pid.getPID3Array(0).getCX4().getHD1().getStringValue();
//            val = pid.getPID3Array(1).getCX1().getStringValue();
//            val = pid.getPID3Array(1).getCX4().getHD1().getStringValue();
//            val = pid.getPID3Array(1).getCX5().getStringValue();
//            val = pid.getPID4Array(0).getCX1().getStringValue();
//            val = pid.getPID5Array(0).getXPN1().getFN1().getStringValue();
//            val = pid.getPID5Array(0).getXPN2().getStringValue();
//            val = pid.getPID5Array(0).getXPN3().getStringValue();
//            val = pid.getPID7().getTS1().getStringValue();
//            val = pid.getPID8().getStringValue();
//            val = pid.getPID11Array(0).getXAD1().getSAD1().getStringValue();
//            val = pid.getPID11Array(0).getXAD3().getStringValue();
//            val = pid.getPID11Array(0).getXAD4().getStringValue();
//            val = pid.getPID11Array(0).getXAD5().getStringValue();
//            val = pid.getPID11Array(0).getXAD6().getStringValue();
//            val = pid.getPID13Array(0).getXTN1().getStringValue();
//            val = pid.getPID14Array(0).getXTN1().getStringValue();
//            val = pid.getPID16().getCE1().getStringValue();
//            val = pid.getPID16().getCE3().getStringValue();
//            val = pid.getPID17().getCE1().getStringValue();
//            val = pid.getPID17().getCE3().getStringValue();
//            val = pid.getPID18().getCX1().getStringValue();
//            val = pid.getPID18().getCX4().getHD1().getStringValue();
//            val = pid.getPID18().getCX5().getStringValue();
//            val = pid.getPID19().getStringValue();
//            val = doc.getADTA01().getPV2().getPV233().getTS1().getStringValue();
//            val = doc.getADTA01().getPV2().getPV28().getTS1().getStringValue();
//            //NK1
//            NK1CONTENT[] nk1s = adtA01.getNK1Array();
//            NK1CONTENT nk1;
//            for (int j = 0; j < nk1s.length; j++) {
//                nk1 = nk1s[j];
//                if (nk1.sizeOfNK14Array() == 0) {
//                    continue;
//                }
//                if (nk1.sizeOfNK12Array() == 0) {
//                    val = nk1.getNK11().getStringValue();
//                    val = nk1.getNK14Array(0).getXAD1().getSAD1().getStringValue();
//                    val = nk1.getNK14Array(0).getXAD3().getStringValue();
//                    val = nk1.getNK14Array(0).getXAD4().getStringValue();
//                    val = nk1.getNK14Array(0).getXAD5().getStringValue();
//                    val = nk1.getNK14Array(0).getXAD6().getStringValue();
//                    val = nk1.getNK16Array(0).getXTN1().getStringValue();
//                    val = nk1.getNK17().getCE1().getStringValue();
//                    val = nk1.getNK17().getCE2().getStringValue();
//                    val = nk1.getNK17().getCE3().getStringValue();
//                    val = nk1.getNK18().getStringValue();
//                    val = nk1.getNK110().getStringValue();
//                    val = nk1.getNK113Array(0).getXON1().getStringValue();
//                } else {
//                    val = nk1.getNK11().getStringValue();
//                    val = nk1.getNK12Array(0).getXPN1().getFN1().getStringValue();
//                    val = nk1.getNK12Array(0).getXPN2().getStringValue();
//                    val = nk1.getNK13().getCE1().getStringValue();
//                    val = nk1.getNK13().getCE3().getStringValue();
//                    val = nk1.getNK14Array(0).getXAD1().getSAD1().getStringValue();
//                    val = nk1.getNK14Array(0).getXAD3().getStringValue();
//                    val = nk1.getNK14Array(0).getXAD4().getStringValue();
//                    val = nk1.getNK14Array(0).getXAD5().getStringValue();
//                    val = nk1.getNK14Array(0).getXAD6().getStringValue();
//                    val = nk1.getNK15Array(0).getXTN1().getStringValue();
//                    val = nk1.getNK16Array(0).getXTN1().getStringValue();
//                    val = nk1.getNK16Array(1).getXTN1().getStringValue();
//                    val = nk1.getNK17().getCE1().getStringValue();
//                    val = nk1.getNK17().getCE2().getStringValue();
//                    val = nk1.getNK17().getCE3().getStringValue();
//                }
//            }
//            //PV1
//            PV1CONTENT pv1 = adtA01.getPV1();
//            val = pv1.getPV12().getStringValue();
//            val = pv1.getPV13().getPL1().getStringValue();
//            val = pv1.getPV17Array(0).getXCN1().getStringValue();
//            val = pv1.getPV17Array(0).getXCN2().getFN1().getStringValue();
//            val = pv1.getPV18Array(0).getXCN1().getStringValue();
//            val = pv1.getPV18Array(0).getXCN2().getFN1().getStringValue();
//            val = pv1.getPV110().getStringValue();
//            val = pv1.getPV117Array(0).getXCN1().getStringValue();
//            val = pv1.getPV117Array(0).getXCN2().getFN1().getStringValue();
//            val = pv1.getPV118().getStringValue();
//            val = pv1.getPV119().getCX1().getStringValue();
//            val = pv1.getPV120Array(0).getFC1().getStringValue();
//            val = pv1.getPV139().getStringValue();
//            val = pv1.getPV144().getTS1().getStringValue();
//            //PV2
//            PV2CONTENT pv2 = adtA01.getPV2();
//            val = pv2.getPV28().getTS1().getStringValue();
//            val = pv2.getPV233().getTS1().getStringValue();
//            //ROL
//            ROLCONTENT rol = adtA01.getROLArray(0);
//            val = rol.getROL2().getStringValue();
//            val = rol.getROL3().getCE1().getStringValue();
//            val = rol.getROL4Array(0).getXCN1().getStringValue();
//            val = rol.getROL4Array(0).getXCN2().getFN1().getStringValue();
//            //OBX
//            OBXCONTENT obx;
//            for (int j = 0; j < 2; j++) {
//                obx = adtA01.getOBXArray(j);
//                val = obx.getOBX2().getStringValue();
//                val = obx.getOBX3().getCE1().getStringValue();
//                val = obx.getOBX3().getCE2().getStringValue();
//                val = obx.getOBX3().getCE3().getStringValue();
//                obx.getOBX5Array(0);
//                val = obx.getOBX6().getCE1().getStringValue();
//                val = obx.getOBX11().getStringValue();
//            }
//            //DG1
//            DG1CONTENT dg1 = adtA01.getDG1Array(0);
//            val = dg1.getDG11().getStringValue();
//            val = dg1.getDG12().getStringValue();
//            val = dg1.getDG13().getCE1().getStringValue();
//            val = dg1.getDG13().getCE2().getStringValue();
//            val = dg1.getDG13().getCE3().getStringValue();
//            val = dg1.getDG16().getStringValue();
//            //GT1
//            GT1CONTENT gt1 = adtA01.getGT1Array(0);
//            val = gt1.getGT11().getStringValue();
//            val = gt1.getGT13Array(0).getXPN1().getFN1().getStringValue();
//            val = gt1.getGT13Array(0).getXPN2().getStringValue();
//            val = gt1.getGT13Array(0).getXPN3().getStringValue();
//            val = gt1.getGT13Array(0).getXPN4().getStringValue();
//            val = gt1.getGT13Array(0).getXPN5().getStringValue();
//            val = gt1.getGT13Array(0).getXPN6().getStringValue();
//            val = gt1.getGT15Array(0).getXAD1().getSAD1().getStringValue();
//            val = gt1.getGT15Array(0).getXAD3().getStringValue();
//            val = gt1.getGT15Array(0).getXAD4().getStringValue();
//            val = gt1.getGT15Array(0).getXAD5().getStringValue();
//            val = gt1.getGT15Array(0).getXAD6().getStringValue();
//            val = gt1.getGT16Array(0).getXTN1().getStringValue();
//            val = gt1.getGT17Array(0).getXTN1().getStringValue();
//            val = gt1.getGT111().getCE1().getStringValue();
//            val = gt1.getGT111().getCE2().getStringValue();
//            val = gt1.getGT111().getCE3().getStringValue();
//            val = gt1.getGT112().getStringValue();
//            val = gt1.getGT116Array(0).getXPN1().getFN1().getStringValue();
//            val = gt1.getGT117Array(0).getXAD1().getSAD1().getStringValue();
//            val = gt1.getGT117Array(0).getXAD3().getStringValue();
//            val = gt1.getGT117Array(0).getXAD4().getStringValue();
//            val = gt1.getGT117Array(0).getXAD5().getStringValue();
//            val = gt1.getGT117Array(0).getXAD6().getStringValue();
//            val = gt1.getGT118Array(0).getXTN1().getStringValue();
//            //IN1
//            IN1CONTENT in1 = adtA01.getADTA01INSURANCEArray(0).getIN1();
//            val = in1.getIN11().getStringValue();
//            val = in1.getIN12().getCE1().getStringValue();
//            val = in1.getIN12().getCE2().getStringValue();
//            val = in1.getIN13Array(0).getCX1().getStringValue();
//            val = in1.getIN14Array(0).getXON1().getStringValue();
//            val = in1.getIN15Array(0).getXAD1().getSAD1().getStringValue();
//            val = in1.getIN15Array(0).getXAD3().getStringValue();
//            val = in1.getIN15Array(0).getXAD4().getStringValue();
//            val = in1.getIN15Array(0).getXAD5().getStringValue();
//            val = in1.getIN17Array(0).getXTN1().getStringValue();
//            val = in1.getIN18().getStringValue();
//            val = in1.getIN114().getAUI1().getStringValue();
//            
//            handler = XmlObject.Factory.newXmlSaxHandler();
//        }
//        long endedAt = System.currentTimeMillis();
//        long elapsed = endedAt - startedAt;
//        if (elapsed < 0) {
//            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
//        }
//        adaptor = null;
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        long mem2 = r.totalMemory() - r.freeMemory();
//        System.out.println("memory used: " + (mem2 - mem1));
//        System.out.println(doc.hashCode());
//        System.out.println(val);
//        System.out.println("End ...");
//        System.out.println("Elapsed: " + elapsed + "ms");
//        if (elapsed == 0) {
//            elapsed = 1;
//        }
//        System.out.println("Characters/ms: "
//                + i * stringData.length() / elapsed);
    }
    
    public void testDecodeIntoXmlObjectFromString(String stringData, int loops,
            boolean doPrint)
        throws XmlException, IOException, SAXException {
        System.out.println("data size (chars) = " + stringData.length());
        
        //Prepare content handler
        XmlSaxHandler handler = XmlObject.Factory.newXmlSaxHandler();
        
        System.out.println("Start ...");
        final int finalLoops = loops;
        int i;
        UnmarshalAdaptor adaptor;
        String val = null;
        int count = 0;
        XmlObject doc = null;
//        Runtime r = Runtime.getRuntime();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        long mem1 = Runtime.getRuntime().totalMemory()
//                        - Runtime.getRuntime().freeMemory();
        long startedAt = System.currentTimeMillis();
        for (i = 0; i < finalLoops; i++) {
            //Prepare UnmarshalAdaptor
            adaptor =
                new UnmarshalAdaptor(mSchemaLocation, mRootElement, 2048);
            adaptor.setContentHandler(handler.getContentHandler());
        
            //Prepare InputSource
            InputSource input = new InputSource(new StringReader(stringData));
            input.setPublicId("string data");
            
            //Do parsing using simple content handler
            adaptor.parse(input);
            
            
            doc = handler.getObject();
            
//            XmlCursor cur = doc.newCursor();
//            boolean hasMove = true;
//            loop1: while (hasMove) {
//                while(cur.toFirstChild()) {
//                    hasMove = true;
//                }
//                val = cur.getTextValue();
//                if (cur.toNextSibling()) {
//                    hasMove = true;
//                    continue loop1;
//                }
//                while(cur.toParent()) {
//                    if (cur.toNextSibling()) {
//                        hasMove = true;
//                        continue loop1;
//                    }
//                }
//                hasMove = false;
//            }
//            cur.dispose();
//            cur = null;
            
            Node node = doc.getDomNode();
            boolean hasMove = true;
            Node next, next1;
            loop1: while (hasMove) {
                while(node.hasChildNodes()) {
                    hasMove = true;
                    node = node.getFirstChild();
                }
                val = node.getNodeValue();
                if ((next = node.getNextSibling()) != null) {
                    node = next;
                    hasMove = true;
                    continue loop1;
                }
                next = node;
                while((next = next.getParentNode()) != null) {
                    if ((next1 = next.getNextSibling()) != null) {
                        node = next1;
                        hasMove = true;
                        continue loop1;
                    }
                }
                hasMove = false;
            }
            node = null;
            
            handler = XmlObject.Factory.newXmlSaxHandler();
        }
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        adaptor = null;
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        r.gc();
//        long mem2 = r.totalMemory() - r.freeMemory();
//        System.out.println("memory used: " + (mem2 - mem1));
        System.out.println("count: " + count);
        System.out.println(val);
        System.out.println(doc.hashCode());
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }
        System.out.println("Characters/ms: "
                + i * stringData.length() / elapsed);
    }
    
    public void testUnmarshalUsingUDOTD(String stringData, int loops,
            boolean doPrint)
        throws XmlException, IOException, SAXException {
        System.out.println("data size (chars) = " + stringData.length());
        
        //Prepare content handler
        ADT_A01 adtA01 = new ADT_A01();
        
        System.out.println("Start ...");
        final int finalLoops = loops;
        int i;
        String val = null;
        int count = 0;
        Runtime r = Runtime.getRuntime();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        long mem1 = Runtime.getRuntime().totalMemory()
                        - Runtime.getRuntime().freeMemory();
        long startedAt = System.currentTimeMillis();
        for (i = 0; i < finalLoops; i++) {
            adtA01.reset();
            adtA01.unmarshalFromString(stringData);
            
            //MSH
            MSH msh = adtA01.getMSH();
            val = msh.getMsh1FieldSeparator();
            val = msh.getMsh2EncodingCharacters();
            val = msh.getMsh3SendingApplication().getHD().getN139NamespaceId();
            val = msh.getMsh4SendingFacility().getHD().getN139NamespaceId();
            val = msh.getMsh5ReceivingApplication().getHD().getN139NamespaceId();
            val = msh.getMsh7DateTimeOfMessage().getTS().getN353Time();
            val = msh.getMsh9MessageType().getMSG().getN188MessageCode();
            val = msh.getMsh9MessageType().getMSG().getN189TriggerEvent();
            val = msh.getMsh9MessageType().getMSG().getN190MessageStructure();
            val = msh.getMsh10MessageControlId();
            val = msh.getMsh11ProcessingId().getPT().getN273ProcessingId();
            val = msh.getMsh12VersionId().getVID().getN362VersionId();
            //EVN
            EVN evn = adtA01.getEVN();
            val = evn.getEvn1EventTypeCode();
            val = evn.getEvn2RecordedDateTime().getTS().getN353Time();
            val = evn.getEvn3DateTimePlannedEvent().getTS().getN353Time();
            val = evn.getEvn4EventReasonCode();
            val = evn.getEvn6EventOccurred().getTS().getN353Time();
            //PID
            PID pid = adtA01.getPID();
            val = pid.getPid3PatientIdentifierList(0).getCX().getN79IdNumber();
            val = pid.getPid3PatientIdentifierList(0).getCX().getN81CheckDigitScheme();
            val = pid.getPid3PatientIdentifierList(0).getCX().getN82AssigningAuthority().getHD().getN139NamespaceId();
            val = pid.getPid3PatientIdentifierList(1).getCX().getN79IdNumber();
            val = pid.getPid3PatientIdentifierList(1).getCX().getN82AssigningAuthority().getHD().getN139NamespaceId();
            val = pid.getPid3PatientIdentifierList(1).getCX().getN83IdentifierTypeCode();
            val = pid.getPid4AlternatePatientIdPid(0).getCX().getN79IdNumber();
            val = pid.getPid5PatientName(0).getXPN().getN418FamilyName().getFN().getN132Surname();
            val = pid.getPid5PatientName(0).getXPN().getN419GivenName();
            val = pid.getPid5PatientName(0).getXPN().getN420SecondAndFurtherGivenNamesOrInitialsThereof();
            val = pid.getPid7DateTimeOfBirth().getTS().getN353Time();
            val = pid.getPid8AdministrativeSex();
            val = pid.getPid11PatientAddress(0).getXAD().getN371StreetAddress().getSAD().getN316StreetOrMailingAddress();
            val = pid.getPid11PatientAddress(0).getXAD().getN373City();
            val = pid.getPid11PatientAddress(0).getXAD().getN374StateOrProvince();
            val = pid.getPid11PatientAddress(0).getXAD().getN375ZipOrPostalCode();
            val = pid.getPid11PatientAddress(0).getXAD().getN376Country();
            val = pid.getPid13PhoneNumberHome(0).getXTN().getN432TelephoneNumber();
            val = pid.getPid14PhoneNumberBusiness(0).getXTN().getN432TelephoneNumber();
            val = pid.getPid16MaritalStatus().getCE().getN23Identifier();
            val = pid.getPid16MaritalStatus().getCE().getN25NameOfCodingSystem();
            val = pid.getPid17Religion().getCE().getN23Identifier();
            val = pid.getPid17Religion().getCE().getN25NameOfCodingSystem();
            val = pid.getPid18PatientAccountNumber().getCX().getN79IdNumber();
            val = pid.getPid18PatientAccountNumber().getCX().getN82AssigningAuthority().getHD().getN139NamespaceId();
            val = pid.getPid18PatientAccountNumber().getCX().getN83IdentifierTypeCode();
            val = pid.getPid19SsnNumberPatient();
            //PV2
            val = adtA01.getPV2().getPv233ExpectedSurgeryDateAndTime().getTS().getN353Time();
            val = adtA01.getPV2().getPv28ExpectedAdmitDateTime().getTS().getN353Time();
            //NK1
            NK1[] nk1s = adtA01.getNK1();
            NK1 nk1;
            for (int j = 0; j < nk1s.length; j++) {
                nk1 = nk1s[j];
                if (nk1.countNk14Address() == 0) {
                    continue;
                }
                if (nk1.countNk12Name() == 0) {
                    val = nk1.getNk11SetIdNk1();
                    val = nk1.getNk14Address(0).getXAD().getN371StreetAddress().getSAD().getN316StreetOrMailingAddress();
                    val = nk1.getNk14Address(0).getXAD().getN373City();
                    val = nk1.getNk14Address(0).getXAD().getN374StateOrProvince();
                    val = nk1.getNk14Address(0).getXAD().getN375ZipOrPostalCode();
                    val = nk1.getNk14Address(0).getXAD().getN376Country();
                    val = nk1.getNk16BusinessPhoneNumber(0).getXTN().getN432TelephoneNumber();
                    val = nk1.getNk17ContactRole().getCE().getN23Identifier();
                    val = nk1.getNk17ContactRole().getCE().getN24Text();
                    val = nk1.getNk17ContactRole().getCE().getN25NameOfCodingSystem();
                    val = nk1.getNk18StartDate();
                    val = nk1.getNk110NextOfKinAssociatedPartiesJobTitle();
                    val = nk1.getNk113OrganizationNameNk1(0).getXON().getN408OrganizationName();
                } else {
                    val = nk1.getNk11SetIdNk1();
                    val = nk1.getNk12Name(0).getXPN().getN418FamilyName().getFN().getN132Surname();
                    val = nk1.getNk12Name(0).getXPN().getN419GivenName();
                    val = nk1.getNk13Relationship().getCE().getN23Identifier();
                    val = nk1.getNk13Relationship().getCE().getN25NameOfCodingSystem();
                    val = nk1.getNk14Address(0).getXAD().getN371StreetAddress().getSAD().getN316StreetOrMailingAddress();
                    val = nk1.getNk14Address(0).getXAD().getN373City();
                    val = nk1.getNk14Address(0).getXAD().getN374StateOrProvince();
                    val = nk1.getNk14Address(0).getXAD().getN375ZipOrPostalCode();
                    val = nk1.getNk14Address(0).getXAD().getN376Country();
                    val = nk1.getNk15PhoneNumber(0).getXTN().getN432TelephoneNumber();
                    val = nk1.getNk16BusinessPhoneNumber(0).getXTN().getN432TelephoneNumber();
                    val = nk1.getNk16BusinessPhoneNumber(1).getXTN().getN432TelephoneNumber();
                    val = nk1.getNk17ContactRole().getCE().getN23Identifier();
                    val = nk1.getNk17ContactRole().getCE().getN24Text();
                    val = nk1.getNk17ContactRole().getCE().getN25NameOfCodingSystem();
                }
            }
            //PV1
            PV1 pv1 = adtA01.getPV1();
            val = pv1.getPv12PatientClass();
            val = pv1.getPv13AssignedPatientLocation().getPL().getN231PointOfCare();
            val = pv1.getPv17AttendingDoctor(0).getXCN().getN385IdNumber();
            val = pv1.getPv17AttendingDoctor(0).getXCN().getN386FamilyName().getFN().getN132Surname();
            val = pv1.getPv18ReferringDoctor(0).getXCN().getN385IdNumber();
            val = pv1.getPv18ReferringDoctor(0).getXCN().getN386FamilyName().getFN().getN132Surname();
            val = pv1.getPv110HospitalService();
            val = pv1.getPv117AdmittingDoctor(0).getXCN().getN385IdNumber();
            val = pv1.getPv117AdmittingDoctor(0).getXCN().getN386FamilyName().getFN().getN132Surname();
            val = pv1.getPv118PatientType();
            val = pv1.getPv119VisitNumber().getCX().getN79IdNumber();
            val = pv1.getPv120FinancialClass(0).getFC().getN130FinancialClassCode();
            val = pv1.getPv139ServicingFacility();
            val = pv1.getPv144AdmitDateTime().getTS().getN353Time();
            //PV2
            PV2 pv2 = adtA01.getPV2();
            val = pv2.getPv28ExpectedAdmitDateTime().getTS().getN353Time();
            val = pv2.getPv233ExpectedSurgeryDateAndTime().getTS().getN353Time();
            //ROL
            ROL rol = adtA01.getROL(0);
            val = rol.getRol2ActionCode();
            val = rol.getRol3RoleRol().getCE().getN23Identifier();
            val = rol.getRol4RolePerson(0).getXCN().getN385IdNumber();
            val = rol.getRol4RolePerson(0).getXCN().getN386FamilyName().getFN().getN132Surname();
            //OBX
            OBX obx;
            for (int j = 0; j < 2; j++) {
                obx = adtA01.getOBX(j);
                val = obx.getObx2ValueType();
                val = obx.getObx3ObservationIdentifier().getCE().getN23Identifier();
                val = obx.getObx3ObservationIdentifier().getCE().getN24Text();
                val = obx.getObx3ObservationIdentifier().getCE().getN25NameOfCodingSystem();
                obx.getObx5ObservationValue(0);
                val = obx.getObx6Units().getCE().getN23Identifier();
                val = obx.getObx11ObservationResultStatus();
            }
            //DG1
            DG1 dg1 = adtA01.getDG1(0);
            val = dg1.getDg11SetIdDg1();
            val = dg1.getDg12DiagnosisCodingMethod();
            val = dg1.getDg13DiagnosisCodeDg1().getCE().getN23Identifier();
            val = dg1.getDg13DiagnosisCodeDg1().getCE().getN24Text();
            val = dg1.getDg13DiagnosisCodeDg1().getCE().getN25NameOfCodingSystem();
            val = dg1.getDg16DiagnosisType();
            //GT1
            GT1 gt1 = adtA01.getGT1(0);
            val = gt1.getGt11SetIdGt1();
            val = gt1.getGt13GuarantorName(0).getXPN().getN418FamilyName().getFN().getN132Surname();
            val = gt1.getGt13GuarantorName(0).getXPN().getN419GivenName();
            val = gt1.getGt13GuarantorName(0).getXPN().getN420SecondAndFurtherGivenNamesOrInitialsThereof();
            val = gt1.getGt13GuarantorName(0).getXPN().getN421SuffixEGJrOrIii();
            val = gt1.getGt13GuarantorName(0).getXPN().getN422PrefixEGDr();
            val = gt1.getGt13GuarantorName(0).getXPN().getN423DegreeEGMd();
            val = gt1.getGt15GuarantorAddress(0).getXAD().getN371StreetAddress().getSAD().getN316StreetOrMailingAddress();
            val = gt1.getGt15GuarantorAddress(0).getXAD().getN373City();
            val = gt1.getGt15GuarantorAddress(0).getXAD().getN374StateOrProvince();
            val = gt1.getGt15GuarantorAddress(0).getXAD().getN375ZipOrPostalCode();
            val = gt1.getGt15GuarantorAddress(0).getXAD().getN376Country();
            val = gt1.getGt16GuarantorPhNumHome(0).getXTN().getN432TelephoneNumber();
            val = gt1.getGt17GuarantorPhNumBusiness(0).getXTN().getN432TelephoneNumber();
            val = gt1.getGt111GuarantorRelationship().getCE().getN23Identifier();
            val = gt1.getGt111GuarantorRelationship().getCE().getN24Text();
            val = gt1.getGt111GuarantorRelationship().getCE().getN25NameOfCodingSystem();
            val = gt1.getGt112GuarantorSsn();
            val = gt1.getGt116GuarantorEmployerName(0).getXPN().getN418FamilyName().getFN().getN132Surname();
            val = gt1.getGt117GuarantorEmployerAddress(0).getXAD().getN371StreetAddress().getSAD().getN316StreetOrMailingAddress();
            val = gt1.getGt117GuarantorEmployerAddress(0).getXAD().getN373City();
            val = gt1.getGt117GuarantorEmployerAddress(0).getXAD().getN374StateOrProvince();
            val = gt1.getGt117GuarantorEmployerAddress(0).getXAD().getN375ZipOrPostalCode();
            val = gt1.getGt117GuarantorEmployerAddress(0).getXAD().getN376Country();
            val = gt1.getGt118GuarantorEmployerPhoneNumber(0).getXTN().getN432TelephoneNumber();
            //IN1
            IN1 in1 = adtA01.getGroupInsurance(0).getIn1();
            val = in1.getIn11SetIdIn1();
            val = in1.getIn12InsurancePlanId().getCE().getN23Identifier();
            val = in1.getIn12InsurancePlanId().getCE().getN24Text();
            val = in1.getIn13InsuranceCompanyId(0).getCX().getN79IdNumber();
            val = in1.getIn14InsuranceCompanyName(0).getXON().getN408OrganizationName();
            val = in1.getIn15InsuranceCompanyAddress(0).getXAD().getN371StreetAddress().getSAD().getN316StreetOrMailingAddress();
            val = in1.getIn15InsuranceCompanyAddress(0).getXAD().getN373City();
            val = in1.getIn15InsuranceCompanyAddress(0).getXAD().getN374StateOrProvince();
            val = in1.getIn15InsuranceCompanyAddress(0).getXAD().getN375ZipOrPostalCode();
            val = in1.getIn17InsuranceCoPhoneNumber(0).getXTN().getN432TelephoneNumber();
            val = in1.getIn18GroupNumber();
            val = in1.getIn114AuthorizationInformation().getAUI().getN9AuthorizationNumber();
        }
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        long mem2 = r.totalMemory() - r.freeMemory();
        System.out.println("memory used: " + (mem2 - mem1));
        System.out.println("count: " + count);
        System.out.println(val);
        System.out.println(adtA01.hashCode());
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }
        System.out.println("Characters/ms: "
                + i * stringData.length() / elapsed);
    }

    public void testDecodeIntoDOMFromString(String stringData, int loops,
            boolean doPrint)
        throws XmlException, IOException, SAXException,
        TransformerFactoryConfigurationError, TransformerException {
        System.out.println("data size (chars) = " + stringData.length());
        
        //Prepare content handler
        XmlObject result = null;
        XmlObject[] results = null;
        
        Transformer transformer =
            TransformerFactory.newInstance().newTransformer();
        System.out.println("Start ...");
        final int finalLoops = loops;
        int i;
        UnmarshalAdaptor adaptor;
        String val = null;
        int count = 0;
        DOMResult domResult;
        Runtime r = Runtime.getRuntime();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        long mem1 = r.totalMemory() - r.freeMemory();
        long startedAt = System.currentTimeMillis();
        for (i = 0; i < finalLoops; i++) {
            //Prepare UnmarshalAdaptor
            adaptor =
                new UnmarshalAdaptor(mSchemaLocation, mRootElement, 2048);
            //Prepare InputSource
            InputSource input = new InputSource(new StringReader(stringData));
            input.setPublicId("string data");

            SAXSource saxSource = new SAXSource(adaptor, input);
            domResult = new DOMResult();
            transformer.transform(saxSource, domResult);
            Node node = domResult.getNode();
            
            //Visit all nodes
            boolean hasMove = true;
            Node next, next1;
            loop1: while (hasMove) {
                while(node.hasChildNodes()) {
                    hasMove = true;
                    node = node.getFirstChild();
                }
                val = node.getNodeValue();
                if ((next = node.getNextSibling()) != null) {
                    node = next;
                    hasMove = true;
                    continue loop1;
                }
                next = node;
                while((next = next.getParentNode()) != null) {
                    if ((next1 = next.getNextSibling()) != null) {
                        node = next1;
                        hasMove = true;
                        continue loop1;
                    }
                }
                hasMove = false;
            }
            domResult = null;
        }
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        long mem2 = r.totalMemory() - r.freeMemory();
        System.out.println("memory used: " + (mem2 - mem1));
        System.out.println("count: " + count);
        System.out.println(val);
        if (result != null) {
            System.out.println(result.xmlText());
        }
        if (results != null) {
            for (i = 0; i < results.length; i++) {
                System.out.println(results[i].xmlText());
            }
        }
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }
        System.out.println("Characters/ms: "
                + i * stringData.length() / elapsed);
    }
    
    public void testDecodeIntoJaxbFromString(String stringData, int loops,
            boolean doPrint) throws JAXBException, IOException, SAXException {
        System.out.println("data size (chars) = " + stringData.length());
        
        //Prepare content handler
        JAXBContext context = JAXBContext.newInstance("org.hl7.v2xml");
        Unmarshaller unmarshaller = context.createUnmarshaller();
        UnmarshallerHandler handler = unmarshaller.getUnmarshallerHandler();
        
        System.out.println("Start ...");
        final int finalLoops = loops;
        int i;
        UnmarshalAdaptor adaptor;
        String val = null;
        JAXBElement<ADTA01CONTENT> doc = null;
        Runtime r = Runtime.getRuntime();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        long mem1 = r.totalMemory() - r.freeMemory();
        long startedAt = System.currentTimeMillis();
        for (i = 0; i < finalLoops; i++) {
            //Prepare UnmarshalAdaptor
            adaptor =
                new UnmarshalAdaptor(mSchemaLocation, mRootElement, 2048);
            adaptor.setContentHandler(handler);
        
            //Prepare InputSource
            InputSource input = new InputSource(new StringReader(stringData));
            input.setPublicId("string data");
            
            doc = unmarshaller.unmarshal(new SAXSource(adaptor, input), ADTA01CONTENT.class);
            
            //Do parsing using simple content handler
            //adaptor.parse(input);
            
            //JAXBElement<ADTA01CONTENT> doc = (JAXBElement<ADTA01CONTENT>) handler.getResult();
            ADTA01CONTENT adtA01 = doc.getValue();
            
//            Iterator<JAXBElement<?>> it = adtA01.getContent().iterator();
//            while (it.hasNext()) {
//                JAXBElement<?> jaxbElem = it.next();
//                if (MSHCONTENT.class.equals(jaxbElem.getDeclaredType())) {
//                    MSHCONTENT msh = (MSHCONTENT) jaxbElem.getValue();
//                    val = msh.getMSH1().getValue();
//                    val = msh.getMSH2().getValue();
//                    val = msh.getMSH3().getHD1().getValue();
//                    val = msh.getMSH4().getHD1().getValue();
//                    val = msh.getMSH5().getHD1().getValue();
//                    val = msh.getMSH7().getTS1().getValue();
//                    val = msh.getMSH9().getMSG1().getValue();
//                    val = msh.getMSH9().getMSG2().getValue();
//                    val = msh.getMSH9().getMSG3().getValue();
//                    val = msh.getMSH10().getValue();
//                    val = msh.getMSH11().getPT1().getValue();
//                    val = msh.getMSH12().getVID1().getValue();
//                } else if (EVNCONTENT.class.equals(jaxbElem.getDeclaredType())) {
//                    EVNCONTENT evn = (EVNCONTENT) jaxbElem.getValue();
//                    val = evn.getEVN1().getValue();
//                    val = evn.getEVN2().getTS1().getValue();
//                    val = evn.getEVN3().getTS1().getValue();
//                    val = evn.getEVN4().getValue();
//                    val = evn.getEVN6().getTS1().getValue();
//                } else if (PIDCONTENT.class.equals(jaxbElem.getDeclaredType())) {
//                    PIDCONTENT pid = (PIDCONTENT) jaxbElem.getValue();
//                    val = pid.getPID3().get(0).getCX1().getValue();
//                    val = pid.getPID3().get(0).getCX3().getValue();
//                    val = pid.getPID3().get(0).getCX4().getHD1().getValue();
//                    val = pid.getPID3().get(1).getCX1().getValue();
//                    val = pid.getPID3().get(1).getCX4().getHD1().getValue();
//                    val = pid.getPID3().get(1).getCX5().getValue();
//                    val = pid.getPID4().get(0).getCX1().getValue();
//                    val = pid.getPID5().get(0).getXPN1().getFN1().getValue();
//                    val = pid.getPID5().get(0).getXPN2().getValue();
//                    val = pid.getPID5().get(0).getXPN3().getValue();
//                    val = pid.getPID7().getTS1().getValue();
//                    val = pid.getPID8().getValue();
//                    val = pid.getPID11().get(0).getXAD1().getSAD1().getValue();
//                    val = pid.getPID11().get(0).getXAD3().getValue();
//                    val = pid.getPID11().get(0).getXAD4().getValue();
//                    val = pid.getPID11().get(0).getXAD5().getValue();
//                    val = pid.getPID11().get(0).getXAD6().getValue();
//                    val = pid.getPID13().get(0).getXTN1().getValue();
//                    val = pid.getPID14().get(0).getXTN1().getValue();
//                    val = pid.getPID16().getCE1().getValue();
//                    val = pid.getPID16().getCE3().getValue();
//                    val = pid.getPID17().getCE1().getValue();
//                    val = pid.getPID17().getCE3().getValue();
//                    val = pid.getPID18().getCX1().getValue();
//                    val = pid.getPID18().getCX4().getHD1().getValue();
//                    val = pid.getPID18().getCX5().getValue();
//                    val = pid.getPID19().getValue();
//                }
//            }
            handler = unmarshaller.getUnmarshallerHandler();
        }
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        adaptor = null;
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        r.gc();
        long mem2 = r.totalMemory() - r.freeMemory();
        System.out.println("memory used: " + (mem2 - mem1));
        System.out.println(val);
        System.out.println(doc.hashCode());
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }
        System.out.println("Characters/ms: "
                + i * stringData.length() / elapsed);
    }

    public void testDecodeFromBytes(byte[] bytes, int loops, boolean doPrint)
            throws XmlException, IOException, SAXException {
        System.out.println("data size (bytes) = " + bytes.length);
        
        //Prepare content handler
        ContentHandler handler =
            new SimpleContentHandler(doPrint);
        
        System.out.println("Start ...");
        long startedAt = System.currentTimeMillis();
        final int finalLoops = loops;
        int i;
        UnmarshalAdaptor adaptor;
        for (i = 0; i < finalLoops; i++) {
            //Prepare UnmarshalAdaptor
            adaptor =
                new UnmarshalAdaptor(mSchemaLocation, mRootElement, bytes.length + 1);
            adaptor.setContentHandler(handler);

            //Prepare InputSource
            InputSource input = new InputSource(
                    new ByteArrayInputStream(bytes));
            input.setEncoding("UTF-8");
            input.setPublicId("bytes data");
            
            //Do parsing using simple content handler
            adaptor.parse(input);
        }
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }
        System.out.println("Bytes/ms: "
                + i * bytes.length / elapsed);
    }
    
    public void testEncodeToString(String stringData,
            int loops, boolean doPrint)
        throws XmlException, IOException,
            TransformerFactoryConfigurationError, TransformerException {
        System.out.println("source data size = " + stringData.length());
        
        System.out.println("Prepare DOM Source ...");
        DOMSource domSource = getDOMData(stringData);
        
        final int finalLoops = loops;
        int i;
        Transformer transformer =
            TransformerFactory.newInstance().newTransformer();
        SAXResult saxResult;
        System.out.println("Start ...");
        long startedAt = System.currentTimeMillis();
        for (i = 0; i < finalLoops; i++) {
            saxResult = new SAXResult(
                    new MarshalHandler(mRootElement, new StringWriter()));
            transformer.transform(domSource, saxResult);
        }
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        
        //Either visiting DOM tree or transforming is very costy, we need to
        //exclude the cost.  We use the SimpleContentHandler to run it this
        //time.  SimpleContentHandler almost does nothing when doPrint is false.
        saxResult = new SAXResult(
                new SimpleContentHandler(false));
        System.out.println("Estimate cost ...");
        startedAt = System.currentTimeMillis();
        for (i = 0; i < finalLoops; i++) {
            transformer.transform(domSource, saxResult);
        }
        endedAt = System.currentTimeMillis();
        long elapsed2 = endedAt - startedAt;
        if (elapsed2 < 0) {
            elapsed2 = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        elapsed = elapsed - elapsed2;
        StringWriter writer = new StringWriter();
        saxResult = new SAXResult(
                new MarshalHandler(mRootElement, writer));
        transformer.transform(domSource, saxResult);
        String resultString = writer.toString();
        if (doPrint) {
            System.out.println(resultString);
        }
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }

        System.out.println("result data size (chars) = "
                + resultString.length());
        System.out.println("Characters/ms: "
                + i * resultString.length() / elapsed);
    }
    
    public void testEncodeToBytes(byte[] bytes, int loops, boolean doPrint)
        throws XmlException, IOException,
            TransformerFactoryConfigurationError, TransformerException {
        System.out.println("source data size (bytes) = " + bytes.length);
        
        System.out.println("Prepare DOM Source ...");
        DOMSource domSource = getDOMData(bytes);
        
        System.out.println("Start ...");
        long startedAt = System.currentTimeMillis();
        final int finalLoops = loops;
        int i;
        Transformer transformer =
            TransformerFactory.newInstance().newTransformer();
        SAXResult saxResult;
        for (i = 0; i < finalLoops; i++) {
            saxResult = new SAXResult(
                    new MarshalHandler(mRootElement,
                            new BufferedOutputStream(
                                    new ByteArrayOutputStream()),
                                        "UTF-8"));
            transformer.transform(domSource, saxResult);
        }
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        
        //Either visiting DOM tree or transforming is very costy, we need to
        //exclude the cost.  We use the SimpleContentHandler to run it this
        //time.  SimpleContentHandler almost does nothing when doPrint is false.
        saxResult = new SAXResult(
                new SimpleContentHandler(false));
        System.out.println("Estimate cost ...");
        startedAt = System.currentTimeMillis();
        for (i = 0; i < finalLoops; i++) {
            transformer.transform(domSource, saxResult);
        }
        endedAt = System.currentTimeMillis();
        long elapsed2 = endedAt - startedAt;
        if (elapsed2 < 0) {
            elapsed2 = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        elapsed = elapsed - elapsed2;
        ByteArrayOutputStream outStream = new ByteArrayOutputStream();
        saxResult = new SAXResult(
                new MarshalHandler(mRootElement,
                        new BufferedOutputStream(outStream),
                                    "UTF-8"));
        transformer.transform(domSource, saxResult);
        byte[] resultBytes = outStream.toByteArray();
        if (doPrint) {
            System.out.println(new String(resultBytes, "UTF-8"));
        }
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }
        System.out.println("result data size (bytes) = " + resultBytes.length);
        System.out.println("result bytes/ms: "
                + i * resultBytes.length / elapsed);
    }

    public void testSAXParsingFromBytes(byte[] bytes, int loops, boolean doPrint)
            throws IOException, ParserConfigurationException, SAXException,
            TransformerException {
        System.out.println("data size (bytes) = " + bytes.length);
        
        //Prepare Parser
        SAXParserFactory pf = SAXParserFactory.newInstance();
        //pf.setFeature("http://xml.org/sax/features/string-interning", false);
        pf.setNamespaceAware(true);
        SAXParser p = pf.newSAXParser();
        DefaultHandler dh = new DefaultHandler();
        
        //Prepare input
        InputStream in = new ByteArrayInputStream(bytes);
        
        System.out.println("Start ...");
        final int finalLoops = loops;
        int i;
        long startedAt = System.currentTimeMillis();
        for (i = 0; i < finalLoops; i++) {
            p.parse(in, dh);
            in.reset();
        }
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        if (doPrint) {
        }
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }
        System.out.println("Bytes/ms: "
                + i * bytes.length / elapsed);
    }
    
    public void testDOMParsingFromBytes(byte[] bytes, int loops, boolean doPrint)
            throws IOException, ParserConfigurationException, SAXException,
            TransformerException {
        System.out.println("data size (bytes) = " + bytes.length);
        
        //Prepare Document Builder
        DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
        dbf.setNamespaceAware(true);
        //dbf.setFeature("http://xml.org/sax/features/string-interning", true);
        dbf.setFeature("http://apache.org/xml/features/dom/defer-node-expansion", false);
        DocumentBuilder db = dbf.newDocumentBuilder();
        
        //Prepare input
        InputStream in = new ByteArrayInputStream(bytes);
        
        final int finalLoops = loops;
        int i;
        Document doc = null;
        String name1 = null;
        String name2 = null;
        String val;
        
        for (i = 0; i < 2; i++) {
            doc = db.parse(in);
            
            Node node = doc.getDocumentElement();
            
            //Visit all nodes
            boolean hasMove = true;
            Node next, next1;
            loop1: while (hasMove) {
                while(node.hasChildNodes()) {
                    hasMove = true;
                    node = node.getFirstChild();
                }
                val = node.getNodeValue();
                if ((next = node.getNextSibling()) != null) {
                    node = next;
                    hasMove = true;
                    continue loop1;
                }
                next = node;
                while((next = next.getParentNode()) != null) {
                    if ((next1 = next.getNextSibling()) != null) {
                        node = next1;
                        hasMove = true;
                        continue loop1;
                    }
                }
                hasMove = false;
            }
            in.reset();
        }
        
        dbf = null;
        db = null;
        doc = null;
        in = null;
        
      Runtime rt = Runtime.getRuntime();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
    rt.gc();
    rt.gc();
    rt.gc();
    rt.gc();
    rt.gc();
    rt.gc();
    rt.gc();
    rt.gc();
    rt.gc();
    rt.gc();
    rt.gc();
    rt.runFinalization();
    rt.runFinalization();
    rt.runFinalization();
    rt.runFinalization();
    rt.runFinalization();
    rt.runFinalization();
    rt.runFinalization();
    rt.runFinalization();
    rt.runFinalization();
    rt.runFinalization();
    rt.runFinalization();
    rt.runFinalization();
    rt.gc();
    rt.gc();
    rt.gc();
    rt.gc();
    rt.gc();
    rt.gc();
    rt.gc();
    rt.gc();
    rt.gc();
    rt.gc();
    rt.gc();
      dbf = DocumentBuilderFactory.newInstance();
      dbf.setNamespaceAware(true);
      //dbf.setFeature("http://xml.org/sax/features/string-interning", true);
      dbf.setFeature("http://apache.org/xml/features/dom/defer-node-expansion", false);
      db = dbf.newDocumentBuilder();
      long mem1 = rt.totalMemory() - rt.freeMemory();
        
      in = new ByteArrayInputStream(bytes);
      
        System.out.println("Start ...");
        long startedAt = System.currentTimeMillis();
        
        for (i = 0; i < finalLoops; i++) {
            doc = db.parse(in);
            
            Node node = doc.getDocumentElement();
            
            //Visit all nodes
            boolean hasMove = true;
            Node next, next1;
            loop1: while (hasMove) {
                while(node.hasChildNodes()) {
                    hasMove = true;
                    node = node.getFirstChild();
                }
                val = node.getNodeValue();
                if ((next = node.getNextSibling()) != null) {
                    node = next;
                    hasMove = true;
                    continue loop1;
                }
                next = node;
                while((next = next.getParentNode()) != null) {
                    if ((next1 = next.getNextSibling()) != null) {
                        node = next1;
                        hasMove = true;
                        continue loop1;
                    }
                }
                hasMove = false;
            }
            in.reset();
        }
        
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        
        in = null;
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      long mem2 = rt.totalMemory() - rt.freeMemory();
      
      System.out.println("mem1: " + mem1);
      System.out.println("mem2: " + mem2);
      System.out.println("memory used: " + (mem2 - mem1));
        if (doPrint) {
            TransformerFactory tf = TransformerFactory.newInstance();
            Transformer t = tf.newTransformer();
            t.transform(new DOMSource(doc), new StreamResult(System.out));
        }
        System.out.print(doc.toString());
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }
        System.out.println("Bytes/ms: "
                + i * bytes.length / elapsed);
    }
    
    public void testXmlBeansParsingFromBytes(byte[] bytes, int loops, boolean doPrint)
            throws IOException, XmlException {
        System.out.println("data size (bytes) = " + bytes.length);
                
        //Prepare input
        InputStream in = new ByteArrayInputStream(bytes);
        
        final int finalLoops = loops;
        int i;
        XmlObject xmlObj = null;
        
        for (i = 0; i < 2; i++) {
            xmlObj = XmlObject.Factory.parse(in);
            in.reset();
        }
        
        in = null;
        xmlObj = null;
        
        Runtime rt = Runtime.getRuntime();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
        long mem1 = rt.totalMemory() - rt.freeMemory();
        
        in = new ByteArrayInputStream(bytes);
        
        System.out.println("Start ...");
        long startedAt = System.currentTimeMillis();
        for (i = 0; i < finalLoops; i++) {
            xmlObj = XmlObject.Factory.parse(in);
            in.reset();
        }
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        
        in = null;
        
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      long mem2 = rt.totalMemory() - rt.freeMemory();
      
      System.out.println("mem1: " + mem1);
      System.out.println("mem2: " + mem2);
      System.out.println("memory used: " + (mem2 - mem1));
      
        if (doPrint) {
            System.out.println(xmlObj.xmlText());
        }
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }
        System.out.println("Bytes/ms: "
                + i * bytes.length / elapsed);
    }

    public void testStAXParsingFromBytes(byte[] bytes, int loops, boolean doPrint)
            throws IOException, XMLStreamException {
        System.out.println("data size (bytes) = " + bytes.length);
        
        XMLInputFactory inf = XMLInputFactory.newInstance();
        
        //Prepare input
        InputStream in = new ByteArrayInputStream(bytes);
                
        System.out.println("Start ...");
        final int finalLoops = loops;
        int i;
        XMLEventReader r;
        long startedAt = System.currentTimeMillis();
        for (i = 0; i < finalLoops; i++) {
            r = inf.createXMLEventReader(in);
            while (r.hasNext()) r.next();
            in.reset();
        }
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        if (doPrint) {
        }
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }
        System.out.println("Bytes/ms: "
                + i * bytes.length / elapsed);
    }

    public void testJavolutionParsingFromBytes(byte[] bytes, int loops, boolean doPrint)
            throws IOException, javolution.xml.stream.XMLStreamException {
        System.out.println("data size (bytes) = " + bytes.length);
        
        javolution.xml.stream.XMLInputFactory inf =
            javolution.xml.stream.XMLInputFactory.newInstance();
        
        //Prepare input
        InputStream in = new ByteArrayInputStream(bytes);
                
        final int finalLoops = loops;
        int i;
        int tp;
        int t = 0;
        Integer token;
        XMLStreamReader r = null;
        CharArray ns;
        CharArray ln;
        int ti = 0;
        FastMap<char[], FastMap<char[], Integer>> nsMap
            = new FastMap<char[], FastMap<char[], Integer>>();
        FastMap<char[], Integer> nameMap;
        ArrayList<char[]> nsList = new ArrayList<char[]>();
        ArrayList<char[]> lnList = new ArrayList<char[]>();
        ArrayList<char[]> values = new ArrayList<char[]>();
        int[] tokens = new int[5000];
        for (i = 0; i < 2; i++) {
            in.reset();
            nsMap.clear();
            values.clear();
            ti = 0;
            r = inf.createXMLStreamReader(in);
            while (r.hasNext()) {
                if ((tp = r.next()) == XMLStreamReader.START_ELEMENT) {
                    ns = r.getNamespaceURI();
                    if ((nameMap = nsMap.get(ns.array())) == null) {
                        nameMap = new FastMap<char[], Integer>();
                        nsMap.put(ns.array(), nameMap);
                    }
                    ln = r.getLocalName();
                    if ((token = nameMap.get(ln.array())) == null) {
                        token = Integer.valueOf(t++);
                        nameMap.put(ln.array(), token);
                    }
                    tokens[ti++] = token;
                } else if (tp == XMLStreamReader.CHARACTERS) {
                    values.add(r.getTextCharacters());
                }
            }
        }
        r = null;
        in = null;
        nsMap.clear();
        values.clear();
        nsMap = null;
        values = null;
        tokens = null;
        nameMap = null;
        ns = null;
        ln = null;
        
        Runtime rt = Runtime.getRuntime();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
        long mem1 = rt.totalMemory() - rt.freeMemory();
        
        in = new ByteArrayInputStream(bytes);
        nsMap = new FastMap<char[], FastMap<char[], Integer>>();
        values = new ArrayList<char[]>();
        tokens = new int[5000];
        
        System.out.println("Start ...");
        long startedAt = System.currentTimeMillis();
        for (i = 0; i < finalLoops; i++) {
            in.reset();
            nsMap.clear();
            values.clear();
            ti = 0;
            r = inf.createXMLStreamReader(in);
            while (r.hasNext()) {
                //r.next();
                if ((tp = r.next()) == XMLStreamReader.START_ELEMENT) {
                    ns = r.getNamespaceURI();
                    if ((nameMap = nsMap.get(ns.array())) == null) {
                        nameMap = new FastMap<char[], Integer>();
                        nsMap.put(ns.array(), nameMap);
                    }
                    ln = r.getLocalName();
                    if ((token = nameMap.get(ln.array())) == null) {
                        token = Integer.valueOf(t++);
                        nameMap.put(ln.array(), token);
                    }
                    tokens[ti++] = token;
                } else if (tp == XMLStreamReader.CHARACTERS) {
                    values.add(r.getTextCharacters());
                }
            }
        }
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        r = null;
        in = null;
        nameMap = null;
        ns = null;
        ln = null;
        
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
        rt.runFinalization();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.runFinalization();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
      rt.gc();
        long mem2 = rt.totalMemory() - rt.freeMemory();
        System.out.println("memory used: " + (mem2 - mem1));
        System.out.println("total tokens = " + ti + ", values = " + values.size());
        if (doPrint) {
        }
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }
        System.out.println("Bytes/ms: "
                + i * bytes.length / elapsed);
    }
    
    public void testVTDParsingFromBytes(byte[] bytes, int loops, boolean doPrint)
            throws EncodingException, EOFException, EntityException,
            ParseException, ModifyException, NavException, IOException,
            TranscodeException {
        System.out.println("data size (bytes) = " + bytes.length);
        
        //Prepare VTD Generator
        VTDGen vtdGen = new VTDGen();
        
        System.out.println("Start ...");
        final int finalLoops = loops;
        int i;
        VTDNav sourceNav = null;
        VTDNav destNav = null;
        XMLModifier modifier = new XMLModifier();
        long startedAt = System.currentTimeMillis();
        for (i = 0; i < finalLoops; i++) {
            vtdGen.setDoc(bytes);
            vtdGen.parse(true);
//            sourceNav = vtdGen.getNav();
//            vtdGen.clear();
//            vtdGen.setDoc("<?xml version=\"1.0\" encoding=\"UTF-8\"?><a><b/></a>".getBytes());
//            vtdGen.parse(true);
//            destNav = vtdGen.getNav();
//            modifier.bind(destNav);
//            sourceNav.toElement(VTDNav.ROOT);
//            sourceNav.toElement(VTDNav.FIRST_CHILD);
//            destNav.toElement(VTDNav.ROOT);
//            destNav.toElement(VTDNav.FIRST_CHILD);
//            modifier.insertAfterElement(sourceNav.getElementFragmentNs());
//            sourceNav.toElement(VTDNav.NEXT_SIBLING);
//            modifier.insertAfterElement(sourceNav.getElementFragmentNs());
//            sourceNav.toElement(VTDNav.NEXT_SIBLING);
//            modifier.insertAfterElement(sourceNav.getElementFragmentNs());
//            sourceNav.toElement(VTDNav.NEXT_SIBLING);
//            modifier.insertAfterElement(sourceNav.getElementFragmentNs());
//            sourceNav.toElementNS(VTDNav.NEXT_SIBLING, "urn:hl7-org:v2xml", "PV1");
//            modifier.insertAfterElement(sourceNav.getElementFragmentNs());
//            sourceNav.toElement(VTDNav.NEXT_SIBLING);
//            modifier.insertAfterElement(sourceNav.getElementFragmentNs());
//            modifier.remove();
//            if (doPrint) {
//                modifier.output(System.out);
//            }
        }
        long endedAt = System.currentTimeMillis();
        long elapsed = endedAt - startedAt;
        if (elapsed < 0) {
            elapsed = (Long.MAX_VALUE - startedAt) + endedAt;
        }
        System.out.println("End ...");
        System.out.println("Elapsed: " + elapsed + "ms");
        if (elapsed == 0) {
            elapsed = 1;
        }
        System.out.println("Bytes/ms: "
                + i * bytes.length / elapsed);
    }
    
    private static SchemaGlobalElement getRootElement(
            File schemaFile, QName rootElementName)
        throws XmlException, IOException {
        
        XmlObject schemaXmlObj =
            XmlObject.Factory.parse(schemaFile);
        SchemaTypeSystem schemaTS =
            XmlBeans.compileXsd(new XmlObject[]{schemaXmlObj},
                XmlBeans.getContextTypeLoader(), null);
        SchemaTypeLoader typeLoader =
            XmlBeans.typeLoaderUnion(
                new SchemaTypeLoader[]{XmlBeans.getContextTypeLoader(),
                        schemaTS});
        SchemaGlobalElement rootElement =
            typeLoader.findElement(rootElementName);
        return rootElement;
    }
    
    private static SchemaGlobalElement getRootElement(
            URL schemaURL, QName rootElementName)
        throws XmlException, IOException {
        
            XmlOptions options = new XmlOptions();
            options.put(XmlOptions.COMPILE_DOWNLOAD_URLS, Boolean.TRUE);
            XmlObject schemaXmlObj =
                XmlObject.Factory.parse(schemaURL, options);
            SchemaTypeSystem schemaTS =
                XmlBeans.compileXsd(new XmlObject[]{schemaXmlObj},
                    XmlBeans.getContextTypeLoader(), options);
            SchemaTypeLoader typeLoader =
                XmlBeans.typeLoaderUnion(
                    new SchemaTypeLoader[]{XmlBeans.getContextTypeLoader(),
                            schemaTS});
            SchemaGlobalElement rootElement =
                typeLoader.findElement(rootElementName);
            return rootElement;
    }
    
    private DOMSource getDOMData(String stringData)
            throws TransformerFactoryConfigurationError, TransformerException {
        StreamSource streamSource = new StreamSource(
                new StringReader(stringData));
        Transformer transformer =
            TransformerFactory.newInstance().newTransformer();
        DOMResult domResult = new DOMResult();
        transformer.transform(streamSource, domResult);
        return new DOMSource(domResult.getNode());
    }
    
    private DOMSource getDOMData(byte[] bytesData)
            throws TransformerFactoryConfigurationError, TransformerException {
        StreamSource streamSource = new StreamSource(
                new ByteArrayInputStream(bytesData));
        Transformer transformer =
            TransformerFactory.newInstance().newTransformer();
        DOMResult domResult = new DOMResult();
        transformer.transform(streamSource, domResult);
        return new DOMSource(domResult.getNode());
    }

    class SimpleContentHandler implements ContentHandler {
        
        private final boolean mDoPrint;
        
        int mIndent = 0;
        boolean mHasChar = false;
        boolean mIsOpen = false;
        
        SimpleContentHandler(boolean doPrint) {
            mDoPrint = doPrint;
        }
        
        public void setDocumentLocator(Locator locator) {
            
        }

        public void startDocument() throws SAXException {
            if (!mDoPrint) {
                return;
            }
            System.out.print(
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
        }

        public void endDocument() throws SAXException {
            if (!mDoPrint) {
                return;
            }
            System.out.println();
        }

        public void startPrefixMapping(String prefix, String uri)
                throws SAXException {
        }

        public void endPrefixMapping(String prefix)
                throws SAXException {
        }

        public final void startElement(String uri,
                String localName, String qName, Attributes atts)
                throws SAXException {
            if (!mDoPrint) {
                return;
            }
            System.out.println();
            if (mIsOpen) {
                mIndent++;
            }
            printIndent();
            System.out.print("<" + localName
                    + " xmlns=\"" + uri + "\"");
            for (int i = 0; i < atts.getLength(); i++) {
                System.out.print(" " + atts.getQName(i));
                System.out.print("=\"");
                System.out.print(atts.getValue(i));
                System.out.print("\"");
            }
            System.out.print(">");
            mHasChar = false;
            mIsOpen = true;
        }

        public final void endElement(String uri, String localName,
                String qName) throws SAXException {
            if (!mDoPrint) {
                return;
            }
            if (mHasChar) {
                System.out.print("</" + localName + ">");
                mHasChar = false;
            } else {
                System.out.println();
                mIndent--;
                printIndent();
                System.out.print("</" + localName + ">");
            }
            mIsOpen = false;
        }

        public final void characters(char[] ch, int start,
                int length) throws SAXException {
            if (!mDoPrint) {
                return;
            }
            String chars = new String(ch, start, length);
            chars = chars.replace("&", "&amp;");
            System.out.print(chars);
            if (chars.length() > 0) {
                mHasChar = true;
            }
        }

        public void ignorableWhitespace(char[] ch, int start,
                int length) throws SAXException {
        }

        public void processingInstruction(String target,
                String data) throws SAXException {
        }

        public void skippedEntity(String name)
                throws SAXException {
        }
        
        private void printIndent() {
            for (int i = 0; i < mIndent; i++) {
                System.out.print("    ");
            }
        }
    }
    
    private static class JarEntityResolver implements EntityResolver {

        public InputSource resolveEntity(String publicId, String systemId)
                throws SAXException, IOException {
            InputStream in = new URL(systemId).openStream();
            InputSource inputSource = new InputSource(in);
            inputSource.setPublicId(publicId);
            inputSource.setSystemId(systemId);
            return inputSource;
        }
    }
    
    public static void main(String[] args) {
        
        String testWhat = "ds";
        int loops = 1;
        boolean printResult = false;
        File testDir =
            new File(System.getProperty("ENCODER_SHAREDLIBRARY") + "hl7/test");
        String inputFileName = null;
        String schemaFileName = null;
        String schemaURLString = null;
        String rootElementName = null;
        for (int i = 0; args != null && i < args.length; i++) {
            if ("-t".equals(args[i])) {
                //test what?
                if (i + 1 < args.length) {
                    testWhat = args[i + 1];
                }
            } else if ("-d".equals(args[i])) {
                //testing directory
                if (i + 1 < args.length) {
                    testDir = new File(args[i + 1]);
                }
            } else if ("-l".equals(args[i])) {
                //number of loops
                if (i + 1 < args.length) {
                    loops = Integer.parseInt(args[i + 1]);
                }
            } else if ("-p".equals(args[i])) {
                //print result?
                printResult = true; 
            } else if ("-i".equals(args[i])) {
                //input file
                if (i + 1 < args.length) {
                    inputFileName = args[i + 1];
                }
            } else if ("-s".equals(args[i])) {
                //schema file using file path
                if (i + 1 < args.length) {
                    schemaFileName = args[i + 1];
                }
            } else if ("-u".equals(args[i])) {
                //schema file using URL
                if (i + 1 < args.length) {
                    schemaURLString = args[i + 1];
                }
            } else if ("-r".equals(args[i])) {
                //root element
                if (i + 1 < args.length) {
                    rootElementName = args[i + 1];
                }
            } else if ("-help".equals(args[i]) || "/?".equals(args[i])) {
                System.out.println("Usage: java SimplePerfTest "
                        + "[-d <testing dir>] "
                        + "[-t <test what, value=(ls, lb, ds, db, es or eb)] "
                        + "[-l <number of loops>] "
                        + "[-p]");
                System.exit(1);
            }
        }
        
        try {
            System.out.println("Load input data ...");
            String stringData = mStringData;
            byte[] bytesData = mBytesData;
            if (inputFileName != null) {
                stringData = UnicodeFile.getText(
                        new File(testDir, inputFileName));
                bytesData = stringData.getBytes("UTF-8");
            }
            
            System.out.println("Prepare XSD ...");
            File schemaFile = null;
            URL schemaURL = null;
            if (schemaFileName == null && schemaURLString == null) {
                schemaFile = new File(testDir, "data/hl7_231_xsd/ADT_A43.xsd");
            } else if (schemaFileName != null) {
                schemaFile = new File(testDir, schemaFileName);
            } else {
                schemaURL = new URL(schemaURLString);
            }
            SchemaGlobalElement rootElement;
            if (rootElementName == null) {
                if (schemaFile != null) {
                    rootElement = getRootElement(schemaFile,  
                        new QName("urn:hl7-org:v2xml", "ADT_A43"));
                } else {
                    rootElement = getRootElement(schemaURL,  
                            new QName("urn:hl7-org:v2xml", "ADT_A43"));
                }
            } else {
                QName qName;
                if (rootElementName.charAt(0) == '{') {
                    int pos = rootElementName.indexOf('}');
                    qName =
                        new QName(rootElementName.substring(1, pos),
                                rootElementName.substring(pos + 1));
                } else {
                    qName = new QName(rootElementName);
                }
                if (schemaFile != null) {
                    rootElement = getRootElement(schemaFile, qName);
                } else {
                    rootElement = getRootElement(schemaURL, qName);
                }
            }
            SimpleTestDriver_binding perfTest;
            if (schemaURL != null) {
                perfTest = new SimpleTestDriver_binding(schemaURL, rootElement);
            } else {
                perfTest = new SimpleTestDriver_binding(schemaFile.toURL(), rootElement);
            }
            if ("ls".equals(testWhat)) {
                perfTest.testLexingFromString(stringData, loops, printResult);
            } else if ("lb".equals(testWhat)) {
                perfTest.testLexingFromBytes(bytesData, loops, printResult);
            } else if ("ds".equals(testWhat)) {
                perfTest.testDecodeFromString(stringData, loops, printResult);
            } else if ("db".equals(testWhat)) {
                perfTest.testDecodeFromBytes(bytesData, loops, printResult);
            } else if ("es".equals(testWhat)) {
                perfTest.testEncodeToString(stringData, loops, printResult);
            } else if ("eb".equals(testWhat)) {
                perfTest.testEncodeToBytes(bytesData, loops, printResult);
            } else if ("dpb".equals(testWhat)) {
                perfTest.testDOMParsingFromBytes(bytesData, loops, printResult);
            } else if ("spb".equals(testWhat)) {
                perfTest.testSAXParsingFromBytes(bytesData, loops, printResult);
            } else if ("xpb".equals(testWhat)) {
                perfTest.testXmlBeansParsingFromBytes(bytesData, loops, printResult);
            } else if ("stpb".equals(testWhat)) {
                perfTest.testStAXParsingFromBytes(bytesData, loops, printResult);
            } else if ("jpb".equals(testWhat)) {
                perfTest.testJavolutionParsingFromBytes(bytesData, loops, printResult);
            } else if ("vpb".equals(testWhat)) {
                perfTest.testVTDParsingFromBytes(bytesData, loops, printResult);
            } else if ("dxs".equals(testWhat)) {
                perfTest.testDecodeIntoXmlBeansFromString(stringData, loops, printResult);
            } else if ("djs".equals(testWhat)) {
                perfTest.testDecodeIntoJaxbFromString(stringData, loops, printResult);
            } else if ("dds".equals(testWhat)) {
                perfTest.testDecodeIntoDOMFromString(stringData, loops, printResult);
            } else if ("dos".equals(testWhat)) {
                perfTest.testDecodeIntoXmlObjectFromString(stringData, loops, printResult);
            } else if ("dus".equals(testWhat)) {
                perfTest.testUnmarshalUsingUDOTD(stringData, loops, printResult);
            } else {
                System.out.println("Usage: java SimplePerfTest "
                        + "[-d <testing dir>] "
                        + "[-t <test what, value=(ls, lb, ds, dxs, db, es or eb)] "
                        + "[-l <number of loops>] "
                        + "[-p]");
                System.exit(1);
            }
        } catch (XmlException e) {
            e.printStackTrace();
            System.exit(2);
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(3);
        } catch (TransformerFactoryConfigurationError e) {
            e.printStackTrace();
            System.exit(4);
        } catch (SAXException e) {
            e.printStackTrace();
            System.exit(5);
        } catch (TransformerException e) {
            e.printStackTrace();
            System.exit(6);
        } catch (JAXBException e) {
            e.printStackTrace();
            System.exit(7);
        } catch (ParserConfigurationException e) {
            e.printStackTrace();
            System.exit(8);
        } catch (EncodingException e) {
            e.printStackTrace();
            System.exit(9);
        } catch (EOFException e) {
            e.printStackTrace();
            System.exit(10);
        } catch (EntityException e) {
            e.printStackTrace();
            System.exit(11);
        } catch (ParseException e) {
            e.printStackTrace();
            System.exit(12);
        } catch (ModifyException e) {
            e.printStackTrace();
            System.exit(13);
        } catch (NavException e) {
            e.printStackTrace();
            System.exit(14);
        } catch (TranscodeException e) {
            e.printStackTrace();
            System.exit(15);
        } catch (XMLStreamException e) {
            e.printStackTrace();
            System.exit(16);
        } catch (javolution.xml.stream.XMLStreamException e) {
            e.printStackTrace();
            System.exit(17);
        }
        System.exit(0);
    }
}
