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
 * Copyright 2007-2008 ZAZ Consulting, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.zaz.jbi.engine.screenscrapingse;

import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.component.endpoint.Endpoint;
import com.sun.jbi.component.endpoint.EndpointFactory;
import com.sun.jbi.crl.util.Util;

import com.zaz.jbi.engine.screenscrapingse.ScreenScrapingseEndpoint.EntryType;
import com.zaz.jbi.engine.screenscrapingse.process.ScriptInfoVO;
import com.zaz.jbi.engine.screenscrapingse.scrconfig.FilterOneWay;
import com.zaz.jbi.engine.screenscrapingse.scrconfig.Input;
import com.zaz.jbi.engine.screenscrapingse.scrconfig.RequestReplyService;
import com.zaz.jbi.engine.screenscrapingse.scrconfig.Scriptconfig;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

import java.io.File;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.management.DeploymentException;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;
import javax.xml.namespace.QName;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.transform.Templates;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamSource;

public class ScreenScrapingseEndpointFactory implements EndpointFactory {

    private static SAXParserFactory mFactory = SAXParserFactory.newInstance();
    private static TransformerFactory mTransformerFactory = TransformerFactory.newInstance();
    private static Logger msLogger = Logger.getLogger(ScreenScrapingseEndpointFactory.class.getName());
    private static final String DESCRIPTOR_FILE = "scriptmap.xml";
    private static final String JAR_FILE = "additionalSchemas.jar";

    /** @see com.sun.jbi.component.endpoint.EndpointFactory#createEndpoint(com.sun.jbi.component.endpoint.EndpointInfo, java.lang.String, java.lang.String) */
    public Endpoint createEndpoint(EndpointInfo info, String serviceUnitName,
            String serviceUnitRootPath) throws DeploymentException {
        ScreenScrapingseEndpoint ept = new ScreenScrapingseEndpoint(info);
        ept.setServiceUnitName(serviceUnitName);
        initEndpoint(ept, info, serviceUnitRootPath);

        return ept;
    }

    /**
     *
     * @param ept
     * @param info
     * @param rootPath
     * @throws javax.jbi.management.DeploymentException
     */
    protected void initEndpoint(ScreenScrapingseEndpoint ept,
            EndpointInfo info, String rootPath) throws DeploymentException {
        try {
            msLogger.log(Level.INFO,
                    "THE ROOT PATH OF THE ENDPOINT IS ::" + rootPath);

            if (msLogger.isLoggable(Level.FINE)) {
                msLogger.fine("Initializing endpoint: " + String.valueOf(info));
            }

            File ScriptsuDescriptorFile = new File(rootPath, DESCRIPTOR_FILE);

            //                    new File(serviceUnitRootPath + File.separator + META_INF_DIR,
            //                     JBI_DESC_FILE_NAME);
            if (!ScriptsuDescriptorFile.exists()) {
                throw error("Script SU descriptor file does not exist: " +
                        ScriptsuDescriptorFile.getAbsolutePath(), null);
            }

            File lJarFile = new File(rootPath, JAR_FILE);

            if (!lJarFile.exists()) {
                msLogger.log(Level.INFO,
                        "Script SU Additional Schemas jar is not there!!: " +
                        lJarFile.getAbsolutePath());
            }

            //Setting the path here ...
            ept.setMAddXSDJarFilePath(lJarFile.getAbsolutePath());
            //Setting the su_install root path
            ept.setMSUInstallPath(rootPath);

            //      DescriptorHandler handler = new DescriptorHandler (info, ept, rootPath);
            //      // TODO double-check newSAXParser() is thread-safe
            //      final SAXParser parser = mFactory.newSAXParser ();
            //      parser.parse (new InputSource (new FileInputStream (ScriptsuDescriptorFile)),
            //          handler);
            ScriptConfigReader lReader = new ScriptConfigReader(info, ept,
                    rootPath);
            lReader.parseScriptMap(ScriptsuDescriptorFile);

            if (msLogger.isLoggable(Level.FINE)) {
                msLogger.fine("Endpoint initialization complete: " +
                        String.valueOf(info));
            }
        } catch (Exception ioe) {
            throw error(
                    "Unexpected I/O error parsing service unit descriptor: " +
                    ioe.getMessage(), ioe);
        }
    }

    private static Templates getTemplates(String rootPath, String fileName) {
        if (fileName == null) {
            return null;
        }

        Templates ret = null;
        File file = new File(rootPath, fileName);
        StreamSource src = new StreamSource(file);

        try {
            synchronized (mTransformerFactory) {
                ret = mTransformerFactory.newTemplates(src);
            }
        } catch (TransformerConfigurationException e) {
            msLogger.log(Level.SEVERE,
                    "Failed to create a new Transformer for " + fileName, e);
        }

        return ret;
    }

    private static DeploymentException error(String message, Exception thrown) {
        if (thrown == null) {
            msLogger.severe(message);

            return new DeploymentException(message);
        } else {
            msLogger.log(Level.SEVERE, message, thrown);

            return new DeploymentException(message, thrown);
        }
    }

    private static class DescriptorHandler extends DefaultHandler {

        private static final String REQUEST_REPLY_ELEMENT = "requestReplyService";
        private static final String FILTER_ONE_WAY_ELEMENT = "filterOneWay";
        private static final String FILTER_REQUEST_REPLY_ELEMENT = "filterRequestReply";
        private static final String INPUT_ELEMENT = "input";
        private static final String OUTPUT_ELEMENT = "output";
        private static final String FILE_ATTR = "file";
        private static final String ENGINE_NAME_ATTR = "engineName";
        private static final String PARTNERLINK_ATTR = "partnerLink";
        private static final String ROLE_NAME_ELEMENT = "roleName";
        private static final String PORTTYPE_ATTR = "portType";
        private static final String OPERATION_ATTR = "operation";
        private static final String MESSAGE_TYPE_ATTR = "messageType";
        private static final String TRANSFORM_JBI_ATTR = "transformJBI";
        private static final String SCRIPT_EXECUTOR = "scriptExecutor";
        private EndpointInfo mInfo = null;
        private ScreenScrapingseEndpoint mEndpoint = null;
        private String mRootPath = null;
        private boolean mHasInvoke = false;
        private boolean mFoundEntry = false;
        private boolean mFoundInvoke = false;

        public DescriptorHandler(EndpointInfo info,
                ScreenScrapingseEndpoint ept, String rootPath) {
            mInfo = info;
            mEndpoint = ept;
            mRootPath = rootPath;
        }

        /** @see org.xml.sax.helpers.DefaultHandler#startElement(java.lang.String, java.lang.String, java.lang.String, org.xml.sax.Attributes) */
        /**
         *  <scriptExecutor>
         *  <input file="greet.rb"
         *  operation ="greeting"
         *  engineName="jruby" />
         *  </scriptExecutor>
         *  Trying the get the details in the above format from the script file.
         */
        public void startElement(String uri, String localName, String qName,
                Attributes attributes) throws SAXException {
            if (mFoundEntry && mFoundInvoke) {
                return;
            }

            if (qName.equals(REQUEST_REPLY_ELEMENT)) {
                mEndpoint.setEntryType(EntryType.REQUEST_REPLY);
                mHasInvoke = mFoundEntry = false;
                mFoundInvoke = true; // don't look for an invoke

            } else if (qName.equals(FILTER_ONE_WAY_ELEMENT)) {
                mEndpoint.setEntryType(EntryType.FILTER_ONE_WAY);
                mHasInvoke = true;
                mFoundInvoke = mFoundEntry = false;
            } else if (qName.equals(FILTER_REQUEST_REPLY_ELEMENT)) {
                mEndpoint.setEntryType(EntryType.FILTER_REQUEST_REPLY);
                mHasInvoke = true;
                mFoundInvoke = mFoundEntry = false;
            } else if (mInfo.isProvides()) {
                if (qName.equals(INPUT_ELEMENT) && isMatch(attributes)) {
                    init(mEndpoint, attributes);
                    mFoundEntry = true;
                } else if (mHasInvoke && mFoundEntry &&
                        qName.equals(OUTPUT_ELEMENT)) {
                    /*
                     * Output element MUST follow Input element!!!
                     */
                    mEndpoint.setInvoke(createInvoke(attributes));
                    mFoundInvoke = true;
                }

                //Prash added
                if (qName.equals(INPUT_ELEMENT)) {
                    String lScrFileName = attributes.getValue(FILE_ATTR);
                    String lEngineName = attributes.getValue(ENGINE_NAME_ATTR);
                    msLogger.log(Level.INFO,
                            "THE SCRIPT FILE NAME TO BE USED**** \n" +
                            lScrFileName);

                    if (lScrFileName != null) {
                        ScriptInfoVO aInfoVo = new ScriptInfoVO();
                        aInfoVo.setMEngineName(lEngineName);
                        aInfoVo.setMScriptFileName(lScrFileName);
                        mEndpoint.setMScrInfo(aInfoVo);
                    } else {
                        msLogger.log(Level.SEVERE,
                                "NO SCRIPT FILE NAME IN SU.. ERRONEOUS Condition Please check\n\n");
                    }
                } //if( qName.equals(INPUT_ELEMENT)) ends.
            //Prash Added ends here

            } //mInfo.isProvides () ends.
            else if (qName.equals(OUTPUT_ELEMENT) && isMatch(attributes)) {
                init(mEndpoint, attributes);
                mFoundEntry = true;
            }
        } //startElement method ends.


        ScreenScrapingseEndpoint createInvoke(Attributes attr) {
            EndpointInfo info = new EndpointInfo(false,
                    String.valueOf(attr.getValue(ROLE_NAME_ELEMENT)),
                    convert(attr.getValue(PORTTYPE_ATTR)),
                    convert(attr.getValue(PARTNERLINK_ATTR)), null); // no link-type specified for Scriptse

            ScreenScrapingseEndpoint ept = new ScreenScrapingseEndpoint(info);
            ept.setEntryType(mEndpoint.getEntryType());
            init(ept, attr);

            return ept;
        }

        boolean isMatch(Attributes attr) {
            String plinkAttr = attr.getValue(PARTNERLINK_ATTR); // service"

            String portAttr = attr.getValue(PORTTYPE_ATTR); // interface

            String roleAttr = attr.getValue(ROLE_NAME_ELEMENT); // endpoint

            return (Util.equals(plinkAttr, convert(mInfo.getServiceName())) &&
                    Util.equals(portAttr, convert(mInfo.getInterfaceName())) &&
                    Util.equals(roleAttr, mInfo.getEndpointName()));
        }

        String convert(QName qname) {
            StringBuffer buff = new StringBuffer();
            buff.append("{").append(qname.getNamespaceURI()).append("}").append(qname.getLocalPart());

            return buff.toString();
        }

        QName convert(String qname) {
            if (qname == null) {
                return null;
            }

            return QName.valueOf(qname);
        }

        void init(ScreenScrapingseEndpoint endpt, Attributes attr) {
            endpt.setOperation(convert(attr.getValue(OPERATION_ATTR)));
            endpt.setMessageType(convert(attr.getValue(MESSAGE_TYPE_ATTR)));

            String jbiTx = attr.getValue(TRANSFORM_JBI_ATTR);
            endpt.setTransformJBI(!Util.isEmpty(jbiTx) &&
                    Boolean.parseBoolean(jbiTx));
            endpt.setTemplates(getTemplates(mRootPath, attr.getValue(FILE_ATTR)));
        }
    } //private static class DescriptorHandler  ends.


    private static class ScriptConfigReader {

        private EndpointInfo mInfo = null;
        private ScreenScrapingseEndpoint mEndpoint = null;
        private String mRootPath = null;
        private boolean mHasInvoke = false;
        private boolean mFoundEntry = false;
        private boolean mFoundInvoke = false;

        public ScriptConfigReader(EndpointInfo info,
                ScreenScrapingseEndpoint ept, String rootPath) {
            mInfo = info;
            mEndpoint = ept;
            mRootPath = rootPath;
        } //ScriptConfigReader ends.


        public void parseScriptMap(File aScrFile) {
            msLogger.log(Level.INFO, "Inside the parseScriptMap Method.");

            try {
                //Creating a JAXB Context.
                JAXBContext lJaxbCtx = JAXBContext.newInstance(com.zaz.jbi.engine.screenscrapingse.scrconfig.ObjectFactory.class);

                //Creating the  UnMarshaller
                Unmarshaller lUnMarshaller = lJaxbCtx.createUnmarshaller();
                Scriptconfig lScrCfg = (Scriptconfig) lUnMarshaller.unmarshal(aScrFile);

                ScriptInfoVO aInfoVo = new ScriptInfoVO();

                aInfoVo.setMScriptCfg(lScrCfg);
                aInfoVo.setMEngineName(lScrCfg.getScrEngineDetails().getEnginename());

                RequestReplyService lReqRepSvc = lScrCfg.getRequestReplyService();
                FilterOneWay lFilterOneWay = lScrCfg.getFilterOneWay();

                //Setting the ScriptFile Name in to the VO.
                if (lReqRepSvc != null) {
                    //Getting the Script File Name.
                    aInfoVo.setMScriptFileName(lScrCfg.getRequestReplyService().getInput().getFile());

                    //Setting other parameters of ScriptEndPoint.
                    mEndpoint.setEntryType(EntryType.REQUEST_REPLY);
                    mHasInvoke = mFoundEntry = false;
                    mFoundInvoke = true; // don't look for an invoke

                    if (mInfo.isProvides()) {
                        Input lInput = lReqRepSvc.getInput();

                        if (isMatch(lInput)) {
                            init(mEndpoint, lInput);
                            mFoundEntry = true;
                        }
                    } // if(mInfo.isProvides() ends.

                } else if (lFilterOneWay != null) {
                    aInfoVo.setMScriptFileName(lFilterOneWay.getInput().getFile());
                    //Setting other parameters of ScriptEndPoint.
                    mEndpoint.setEntryType(EntryType.FILTER_ONE_WAY);
                    mHasInvoke = true;
                    mFoundInvoke = mFoundEntry = false;

                    if (mInfo.isProvides()) {
                        Input lInput = lFilterOneWay.getInput();

                        if (isMatch(lInput)) {
                            init(mEndpoint, lInput);
                            mFoundEntry = true;
                        }

                    //            Output lOp = lFilterOneWay.getOutput();
                    //            if (mHasInvoke && mFoundEntry )
                    //            {
                    //                    /*
                    //                     * Output element MUST follow Input element!!!
                    //                     */
                    //              mEndpoint.setInvoke(createInvoke(lOp));
                    //              mFoundInvoke = true;
                    //            }
                    } // if(mInfo.isProvides() ends.

                } else {
                    msLogger.log(Level.INFO,
                            "NONE OF THE VALID MESSAGES TYPES ARE SET...PLEASE CHECK..");
                }

                mEndpoint.setMScrInfo(aInfoVo);
            } catch (Exception ex) {
                ex.printStackTrace();
                msLogger.log(Level.SEVERE,
                        "Failed to Parse the File" + ex.toString());
            }
        } //parseScriptMap method ends.


        QName convert(String qname) {
            if (qname == null) {
                return null;
            }

            return QName.valueOf(qname);
        }

        String convert(QName qname) {
            StringBuffer buff = new StringBuffer();
            buff.append("{").append(qname.getNamespaceURI()).append("}").append(qname.getLocalPart());

            return buff.toString();
        }

        boolean isMatch(Input aIp) {
            String plinkAttr = aIp.getPartnerLink(); // service"

            String portAttr = aIp.getPortType(); // interface

            String roleAttr = aIp.getRoleName(); // endpoint

            msLogger.log(Level.INFO, "plinkAttr :: " + plinkAttr);
            msLogger.log(Level.INFO, "portAttr :: " + portAttr);
            msLogger.log(Level.INFO, "roleAttr :: " + roleAttr);

            msLogger.log(Level.INFO,
                    "convert(mInfo.getServiceName()) :: " +
                    convert(mInfo.getServiceName()));
            msLogger.log(Level.INFO,
                    "convert(mInfo.getInterfaceName()) :: " +
                    convert(mInfo.getInterfaceName()));
            msLogger.log(Level.INFO,
                    "mInfo.getEndpointName() :: " + mInfo.getEndpointName());

            return (Util.equals(plinkAttr, convert(mInfo.getServiceName())) &&
                    Util.equals(portAttr, convert(mInfo.getInterfaceName())) &&
                    Util.equals(roleAttr, mInfo.getEndpointName()));
        }

        void init(ScreenScrapingseEndpoint endpt, Input aIp) {
            msLogger.log(Level.INFO, "Actuall initialization of END POINT..");
            endpt.setOperation(convert(aIp.getOperation()));
            endpt.setMessageType(convert(aIp.getMessageType()));

            endpt.setTransformJBI(aIp.isTransformJBI());
            //      endpt.setTemplates (getTemplates (mRootPath,aIp.getFile()));
            endpt.print();
        }

        //    void init(ScreenScrapingseEndpoint endpt, Output aOp)
        //    {
        //       endpt.setOperation (convert (aOp.getOperation()));
        //      endpt.setMessageType (convert (aOp.getMessageType()));
        //
        //      endpt.setTransformJBI (aOp.isTransformJBI());
        //      endpt.setTemplates (getTemplates (mRootPath,aOp.getFile()));
        //    }

        //     ScreenScrapingseEndpoint createInvoke (Output aOp)
        //    {
        //      EndpointInfo info =
        //          new EndpointInfo (false,
        //          String.valueOf (aOp.getRoleName() ),
        //          convert (aOp.getPortType()),
        //          convert (aOp.getPartnerLink()),
        //          null); // no link-type specified for Scriptse
        //      ScreenScrapingseEndpoint ept = new ScreenScrapingseEndpoint (info);
        //      ept.setEntryType (mEndpoint.getEntryType ());
        //      init (ept, aOp);
        //      return ept;
        //    }
    } // private static class ScriptConfigReader ends.

} //class ScreenScrapingseEndpointFactory  ends.
