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
 * @(#)ScriptseEndpointManager.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */
package com.sun.jbi.engine.scriptse.jbiadapter;

import java.io.File;
import java.io.FileReader;
import java.io.File;

import javax.jbi.component.ComponentContext;
import javax.jbi.management.DeploymentException;
import javax.jbi.servicedesc.ServiceEndpoint;
import javax.wsdl.Definition;
import javax.wsdl.Operation;
import javax.wsdl.PortType;
import javax.wsdl.xml.WSDLReader;
import javax.xml.namespace.QName;
import javax.xml.parsers.SAXParserFactory;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.Unmarshaller;
import javax.xml.transform.Templates;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamSource;

import org.xml.sax.InputSource;

import com.sun.jbi.common.classloader.CustomClassLoaderUtil.SwitchType;
import com.sun.jbi.common.descriptor.EndpointInfo;
import com.sun.jbi.common.descriptor.ServiceUnit;
import com.sun.jbi.component.toolkit.endpoint.Endpoint;
import com.sun.jbi.component.toolkit.endpoint.EndpointManager;
import com.sun.jbi.component.toolkit.endpoint.impl.AbstractEndpointManager;
import com.sun.jbi.common.util.Util;

import com.sun.wsdl4j.ext.WSDL4JExt;


import java.util.logging.Logger;

import com.sun.jbi.engine.scriptse.jbiadapter.ScriptseEndpoint.EntryType;
import com.sun.jbi.engine.scriptsecore.process.ScriptInfoVO;
import com.sun.jbi.engine.scriptsecore.scrconfig.FilterOneWay;
import com.sun.jbi.engine.scriptsecore.scrconfig.Input;
import com.sun.jbi.engine.scriptsecore.scrconfig.RequestReplyService;
import com.sun.jbi.engine.scriptsecore.scrconfig.Scriptconfig;


public class ScriptseEndpointManager extends AbstractEndpointManager {

    private static SAXParserFactory mFactory = SAXParserFactory.newInstance();
    private static TransformerFactory mTransformerFactory = TransformerFactory.newInstance();
    private static final String DESCRIPTOR_FILE = "scriptmap.xml";
    private static final String JAR_FILE = "additionalSchemas.jar";

    /**
     * Constructs an <code>AleSEEndpointManager</code>.
     */
    public ScriptseEndpointManager(ComponentContext ctx) {
        super(ctx);

    }

    /** @see com.sun.jbi.component.toolkit.endpoint.EndpointManager#createEndpoint(com.sun.jbi.common.descriptor.EndpointInfo, com.sun.jbi.common.descriptor.ServiceUnit) */
    public Endpoint<ScriptInfoVO> createEndpoint(EndpointInfo info, ServiceUnit srvcUnit)
            throws DeploymentException {
        getContext().getCustomClassLoaderUtil().switchClassLoader(srvcUnit.getName(), SwitchType.service_classloader);
        ScriptseEndpoint ept = new ScriptseEndpoint(info);

        //initialize Endpoint

        String rootPath = srvcUnit.getRootPath();
        File ScriptsuDescriptorFile = new File(rootPath, DESCRIPTOR_FILE);
        if (!ScriptsuDescriptorFile.exists()) {
            throw new DeploymentException("Script SU descriptor file does not exist: " +
                    ScriptsuDescriptorFile.getAbsolutePath(), null);
        }
        File lJarFile = new File(rootPath, JAR_FILE);

        if (!lJarFile.exists()) {
            String infomsg = I18n.loc("SCPTSE-6004 Script SU Additional Schemas jar is not there!!: {0}", lJarFile.getAbsolutePath());
            log().info(infomsg);
        }

        //Setting the path here ...
        ept.setMAddXSDJarFilePath(lJarFile.getAbsolutePath());
        //Setting the su_install root path
        ept.setMSUInstallPath(rootPath);
        ScriptConfigReader lReader = new ScriptConfigReader(info, ept,
                rootPath);
        lReader.parseScriptMap(ScriptsuDescriptorFile);



        return ept;
    }

    /** @see com.sun.jbi.component.endpoint.impl.DefaultEndpointManager#addServiceUnit(com.sun.jbi.common.descriptor.ServiceUnit) */
    public void addServiceUnit(ServiceUnit srvcUnit) throws DeploymentException {
        if (srvcUnit == null) {
            return;        // register SUs by ServiceEndpoint
        }
        super.addServiceUnit(srvcUnit);
    }

    /** @see com.sun.jbi.component.toolkit.endpoint.impl.AbstractEndpointManager#lookupServiceUnit(javax.jbi.servicedesc.ServiceEndpoint) */
    public ServiceUnit lookupServiceUnit(ServiceEndpoint endpt) {
        return super.lookupServiceUnit(endpt);
    }

    protected Definition readWsdl(File file) throws Exception {
        WSDLReader reader = WSDL4JExt.newWSDLReader(null);
        return reader.readWSDL(file.toURI().toString(),
                new InputSource(new FileReader(file)));
    }

    private  class ScriptConfigReader {

        private EndpointInfo mInfo = null;
        private ScriptseEndpoint mEndpoint = null;
        private String mRootPath = null;
        private boolean mHasInvoke = false;
        private boolean mFoundEntry = false;
        private boolean mFoundInvoke = false;

        public ScriptConfigReader(EndpointInfo info,
                ScriptseEndpoint ept, String rootPath) {
            mInfo = info;
            mEndpoint = ept;
            mRootPath = rootPath;
        } //ScriptConfigReader ends.

        public void parseScriptMap(File aScrFile) {
            String infomsg = I18n.loc("SCPTSE-6005 Inside the parseScriptMap Method.");
            
            log().info(infomsg);

            try {
                //Creating a JAXB Context.
                JAXBContext lJaxbCtx = JAXBContext.newInstance(com.sun.jbi.engine.scriptsecore.scrconfig.ObjectFactory.class);

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
                    //log().info("NONE OF THE VALID MESSAGES TYPES ARE SET...PLEASE CHECK..");
                }

                mEndpoint.setMScrInfo(aInfoVo);
            } catch (Exception ex) {
               String errormsg=I18n.loc("SCPTSE-6006: Failed to Parse the File {0} ",ex.getMessage() );
               log().warning(errormsg);

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




            return (Util.equals(plinkAttr, convert(mInfo.getServiceName())) &&
                    Util.equals(portAttr, convert(mInfo.getInterfaceName())) &&
                    Util.equals(roleAttr, mInfo.getEndpointName()));
        }

        void init(ScriptseEndpoint endpt, Input aIp) {
          
            endpt.setOperation(convert(aIp.getOperation()));
            endpt.setMessageType(convert(aIp.getMessageType()));

            endpt.setTransformJBI(aIp.isTransformJBI());
            //      endpt.setTemplates (getTemplates (mRootPath,aIp.getFile()));
           
        }        //    void init(ScreenScrapingseEndpoint endpt, Output aOp)
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
}
