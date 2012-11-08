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
 * @(#)AspectsBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.aspects.manager.framework.generic;

import com.sun.data.provider.FieldKey;
import com.sun.data.provider.RowKey;
import com.sun.data.provider.TableDataProvider;
import com.sun.data.provider.impl.ObjectListDataProvider;
import com.sun.jbi.cam.common.FileUtilities;
import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.common.ReadWriteTextFile;
import com.sun.jbi.cam.common.UUIDGenerator;
import com.sun.jbi.cam.common.XMLUtils;
import com.sun.jbi.cam.common.ZipFileUtility;
import com.sun.jbi.cam.manager.framework.common.BaseBean;
import com.sun.jbi.cam.manager.framework.common.FacesUtil;
import com.sun.jbi.cam.manager.framework.generic.ServiceUnitsBean;
import com.sun.jbi.cam.model.management.JBIServiceAssemblyStatus;
import com.sun.jbi.cam.plugins.aspects.common.AspectsGenericConstants;
import com.sun.jbi.cam.plugins.aspects.common.resources.Messages;
import com.sun.jbi.cam.plugins.aspects.support.model.catalog.Catalog;
import com.sun.jbi.cam.plugins.aspects.support.model.catalog.WSDLService;
import com.sun.jbi.cam.plugins.aspects.support.model.catalog.xml.CatalogReader;
import com.sun.jbi.cam.plugins.aspects.support.model.policygroup.PolicyGroup;
import com.sun.jbi.cam.plugins.aspects.support.model.policygroup.PolicyGroupCollection;
import com.sun.jbi.cam.plugins.aspects.support.model.policygroup.xml.AspectPolicyGroupReader;
import com.sun.jbi.cam.plugins.aspects.support.model.policygroup.xml.AspectPolicyGroupWriter;
import com.sun.jbi.cam.plugins.aspects.web.ajax.ServiceSupport;
import com.sun.webui.jsf.component.TableRowGroup;
import com.sun.webui.jsf.model.*;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.net.InetAddress;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.*;
import java.util.zip.ZipEntry;
import javax.faces.event.ActionEvent;
import javax.servlet.ServletContext;
import javax.xml.parsers.ParserConfigurationException;
import org.apache.xmlbeans.XmlException;
import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model.WSDLModel;
import com.sun.jbi.cam.plugins.aspects.support.wsdlxsd.model.WSDLModelHelper;
import org.xml.sax.SAXException;

/**
 * @author ylee
 */       

/**
 * @bugs
 *      20070405 - Unable to retrieve TableDataProvider data value - e.g. "#{data.value.fieldname}" would cause a parsing error
 *
 *          
 */
public class AspectsBean extends BaseBean implements Serializable {
    private static final long serialVersionUID = 1L;
    private static Logger logger = Logger.getLogger(AspectsBean.class.getName());

    private static String POLICYGROUP_ADD = "policyGroupAdd";
    private static String POLICYGROUP_EDIT = "policyGroupEdit";
    private static String POLICYGROUP_DELETE = "policyGroupDelete";
    private static String POLICYGROUP_NEXT = "policyGroupNext";
    private static String POLICYGROUP_CANCEL = "policyGroupCancel";
    private static String POLICYGROUP_DEFAULT_NAME = "PolicyGroup1";

    private static String SERVICE_ADD = "serviceAdd";
    private static String SERVICE_EDIT = "serviceEdit";
    private static String SERVICE_DELETE = "serviceDelete";
    private static String SERVICE_NEXT = "serviceNext";
    private static String SERVICE_SAVE = "serviceSave";
    private static String SERVICE_CANCEL = "serviceCancel";
    private static String SERVICE_GROUP_DEFAULT_NAME = "ServiceGroup1";
    
    private static String WSDL_ADD = "wsdlAdd";
    private static String WSDL_EDIT = "wsdlEdit";
    private static String WSDL_DELETE = "wsdlDelete";
    private static String WSDL_NEXT = "wsdlNext";

    private static String POLICYGROUP_NAME = "policyGroupName";
    private static String INPUTSTRING_NAME = "inStr";
    private static String SERVICE_GROUP_NAME = "serviceGroupName";
    private static String WSDL_NAME = "wsdlName";

    private static String INPUT_SERVICE_STRING = "inServiceStr";
    private static String INPUT_FACADE_STRING = "inFacadeStr";
    private static String INPUT_ASPECT_STRING = "inAspStr";
    
    
    private static String ALERT_TYPE_ERROR = "error";
    private static String ALERT_TYPE_SUCCESS = "success";
    private static String ALERT_TYPE_WARNING = "warning";
    private static String ALERT_TYPE_INFO = "info";
    
    /** Holds value of property alertDetail. */
    transient private String alertMessage = null;
  
    /** Holds value of property alertRendered. */
    transient private boolean renderAlertMessage = false;
    
    transient private String alertType = null;
    transient private String alertSummary;    
    
    private String policyGroupName;
    private String serviceGroupName;
    private String wsdlName;
    private boolean policyGroupAdd = false;
    private boolean serviceAdd = false;
    
    private UploadedFile wsdlUploadedFile;            
    private String wsdlUrl;
    private String wsdlFileName;
    private String wsdlDetail;
    private String serviceFolderName;
    private boolean rbUrlSelected = false;
    private boolean rbFolderSelected = true;

    
    private List<DisplayPolicyGroup> policyGroupList = new ArrayList<DisplayPolicyGroup>();
    private List<DisplayService> serviceList = new ArrayList<DisplayService>();
    private List<DisplayWsdl> wsdlList = new ArrayList<DisplayWsdl>();
    
    private Catalog catalog = null;
    private String baseLocation = null;
    private Group wsdlGroup = null;
    
    
    /** Creates a new instance of AspectsBean */
    public AspectsBean() {
    	setup();
        catalog = new Catalog();
    }
    
    
    public TableDataProvider getPolicyGroups() {
        clearPolicyGroups();
        getPolicyGroupsData();
        provider = new ObjectListDataProvider(policyGroupList);
        return provider;
    }

    
    public TableDataProvider getServices() {
        getServicesFromCatalog();
        provider = new ObjectListDataProvider(serviceList);
        return provider;
    }
    
    
    public Group getWsdlGroup() {
        //logger.info(">>>> getting wsdlGroup..."+wsdlGroup);
        //logger.info(">>>> wsdlList:"+wsdlList);
        if ( wsdlGroup==null ) {
            wsdlGroup = new Group(wsdlList);
        }
        return wsdlGroup;
    }
    
    
    public TableDataProvider getServiceWsdls() {
        provider = new ObjectListDataProvider(wsdlList);
        return provider;
    }    

    public TableDataProvider getTableDataProvider() {
        //logger.info("## getTableDataProvider...");
        return getServiceWsdls();
    }
    
    /**
     * add a new policy group
     */
    public String policyGroupAdd() {
        logger.info("add policy group...");
        clearPolicyGroup();
        setPolicyGroupName(POLICYGROUP_DEFAULT_NAME);
        return POLICYGROUP_ADD;
    }
    
    /**
     * edit a policy group
     */
    /*
    public String policyGroupEdit() {
        logger.info("edit policy group...");
        return POLICYGROUP_EDIT;
    }*/
    
    /**
     * delete a policy group
     */
    public String policyGroupDelete() {
        logger.info("delete policy group...");
        return POLICYGROUP_DELETE;
    }

    
    /**
     * set the policy group name
     * @param value     -   policy name
     */
    public void setPolicyGroupName(String value) {
        policyGroupName = value;
    }
    
    /**
     * get the policy group name
     */ 
    public String getPolicyGroupName() {
        return policyGroupName;
    }
    
    public void policyGroupLink(ActionEvent event) {
        
        logger.info("policy group link...");

        String input = getWebServicesString();
        setParameter(INPUTSTRING_NAME,input);
        
        // @todo - set the current policyGroupName...
        if ( provider!=null ) {
            RowKey rowKey = FacesUtil.getTableRow("aspects");
            policyGroupName= (String)provider.getValue(provider.getFieldKey("name"),rowKey);
            logger.info("policyGroupName: "+policyGroupName);
        }
        setParameter(POLICYGROUP_NAME,policyGroupName);
    }

    /**
     * edit a policy group
     */
    public String policyGroupEdit() {
        logger.info("policy group edit...");
        setHostname();
        //String input = testRetrieveURLs();
        String input = getWebServicesString();
        setParameter(INPUTSTRING_NAME,input);
        String policyGroupFileString = "";
        AspectPolicyGroupReader reader = null;
        PolicyGroupCollection collection = null;
        PolicyGroup group = null;
        try {
            policyGroupFileString = getBaseLocation()+File.separator+AspectPolicyGroupWriter.FILE_NAME_KEY;
            reader = AspectPolicyGroupReader.parseFromFile(policyGroupFileString);
            collection = reader.getCollection();
            group = collection.findPolicyGroup(policyGroupName);
            setParameter(POLICYGROUP_NAME, group.getPolicyGroupName());
            setParameter(INPUT_SERVICE_STRING, group.getInputString());
            setParameter(INPUT_FACADE_STRING, group.getFacadeString());
            setParameter(INPUT_ASPECT_STRING, group.getAspectString());
        } catch (MalformedURLException ex) {
            ex.printStackTrace();
        } catch (ParserConfigurationException ex) {
            ex.printStackTrace();
        } catch (SAXException ex) {
            ex.printStackTrace();
        } catch (URISyntaxException ex) {
            ex.printStackTrace();
        } catch (IOException ex) {
            ex.printStackTrace();
        }
        
         return this.POLICYGROUP_EDIT;
    }
    
    public String policyGroupNext() {
        logger.info("policy group next...");
        setHostname();
        String input = getWebServicesString();
        setParameter(INPUTSTRING_NAME,input);
        setParameter(POLICYGROUP_NAME,policyGroupName);        
        return POLICYGROUP_NEXT;
    } 
    
    public String testRetrieveURLs() {
        ServiceSupport support = new ServiceSupport();
        List<String> wsdlUriList = new ArrayList<String>();
        wsdlUriList.add("http://webservices.amazon.com/AWSECommerceService/AWSECommerceService.wsdl");
        wsdlUriList.add("http://www.weather.gov/forecasts/xml/SOAP_server/ndfdXMLserver.php?wsdl");
        wsdlUriList.add("http://schemas.monster.com/current/wsdl/MonsterBusinessGateway.wsdl");
        wsdlUriList.add("http://www.xmethods.net/sd/2001/CurrencyExchangeService.wsdl");
        wsdlUriList.add("http://terraservice.net/TerraService.asmx?WSDL");
        return support.retrieveServiceInformation(wsdlUriList);
    }
    
    public String policGroupCancel() {
        logger.info("policy group cancel...");
        return POLICYGROUP_CANCEL;
    } 
    
    /**
     * add a new Service
     */
    public String serviceAdd() {
        logger.info("add service...");
        serviceAdd = true;
        clearWsdls();
        reset();
        setServiceFolderName();
        setServiceGroupName(SERVICE_GROUP_DEFAULT_NAME);
        return SERVICE_ADD;
    }
    
    /**
     * edit a Service
     */
    public String serviceEdit() {
        logger.info("edit service...");
        serviceAdd = false;
        clearWsdls();
        // 1. get wsdl service from catalog
        getWsdlsFromCatalog();
        return SERVICE_EDIT;
    }
    
    /**
     * delete a Service
     */
    public String serviceDelete() {
        logger.info("delete service...");
        return SERVICE_DELETE;
    }    
    
    public void serviceLink(ActionEvent event) {
        logger.info("service link...");
        // 1. set the current serviceGroupName...
        if ( provider!=null ) {
            RowKey rowKey = FacesUtil.getTableRow("catalog");
            serviceGroupName= (String)provider.getValue(provider.getFieldKey("name"),rowKey);
            logger.info("serviceName: "+serviceGroupName);
        }
        setParameter(SERVICE_GROUP_NAME,serviceGroupName);
        // 2. retrieve Service Group data from catalog
        // @todo
    }
    
    public String serviceSave() {
        logger.info("service save...");
        // 1. add to table display
        if ( serviceAdd ) {
            DisplayService displayService = new DisplayService(serviceGroupName);
            serviceList.add(displayService);
        }
        // @todo - case for add/edit
        saveToCatalog();
        
        return SERVICE_SAVE;
    }
    
    public String serviceCancel() {
        logger.info("service cancel...");
        // @todo - cancel edits
        return SERVICE_CANCEL;
    }
    
    /**
     * set ServiceGroupName
     * @param serviceGroupName
     */
    public void setServiceGroupName(String serviceName) {
        this.serviceGroupName = serviceName;
    }
    
    public String getServiceGroupName() {
        return serviceGroupName;
    }
    
    public String wsdlAdd() {
        logger.info("wsdl add...");
        return WSDL_ADD;
    }

    public String wsdlEdit() {
        logger.info("wsdl edit...");
        return WSDL_EDIT;
    }

    public String wsdlDelete() {
        logger.info("wsdl delete...");
        /******
        if ( wsdlGroup!=null ) {
            provider = wsdlGroup.getData();
            //Select select = wsdlGroup.getSelect();
            if ( provider!=null ) {
                
                if ( wsdlGroup.getSelect().isKeepSelected() ) {
                    // If we got here, then we're maintaining state across pages.
                    delete(wsdlGroup.getTableRowGroup().getSelectedRowKeys(),wsdlGroup);
                } else {
                    // If we got here, then we're using the phase listener and must
                    // take filtering, sorting, and pagination into account.
                    TableRowGroup tableRowGroup = wsdlGroup.getTableRowGroup();
                    logger.info("## tableRowGroup: "+tableRowGroup);
                    if ( tableRowGroup!=null ) {
                        RowKey[] rowKeys = tableRowGroup.getRenderedSelectedRowKeys();
                        //System.out.println("## rowkeys from tableRowGroup= "+rowKeys);
                        delete(rowKeys,wsdlGroup);
                        // remove from wsdlList
                    }
                }                
            }
        }
         ******/
        return WSDL_DELETE;
    }
    
    public boolean getWsdlSelected() {
        if ( provider!=null ) {
            RowKey rowKey = FacesUtil.getTableRow("data");
            logger.info(">>>> getWsdlSelected - rowid="+rowKey.getRowId());
        }
        return true;
    }
    
    public void setWsdlSelected() {
        if ( provider!=null ) {
            RowKey rowKey = FacesUtil.getTableRow("data");
            logger.info(">>>> setWsdlSelected rowid="+rowKey.getRowId());
            //Boolean selected = (Boolean)provider.getValue(provider.getFieldKey("selected"),rowKey);
        }        
    }
    
    public void setWsdlUrl(String url) {
        wsdlUrl = url;
        //logger.info(">>>> setting wsdlUrl: "+wsdlUrl);
    }
    
    public String getWsdlUrl() {
        //logger.info(">>>> getting wsdlUrl: "+wsdlUrl);
        return wsdlUrl;
    }

    public void wsdlLink(ActionEvent event) {
        logger.info("wsdl link...");
        //
        if ( wsdlGroup!=null ) {
            //provider = wsdlGroup.getData();
            if ( provider!=null ) {
                RowKey rowKey = FacesUtil.getTableRow("data");
                wsdlName= (String)provider.getValue(provider.getFieldKey("name"),rowKey);
                logger.info("wsdl Name: "+wsdlName);
                //wsdlDetail = wsdlName;
                File file = new File(getWsdlServiceLocation(),wsdlName);
                XMLUtils xmlUtils = new XMLUtils();
                try {
                    xmlUtils.parse(file);
                    ByteArrayOutputStream out = new ByteArrayOutputStream();
                    xmlUtils.format2(out);
                    wsdlDetail = out.toString();
                } catch(Exception e) {
                    e.printStackTrace();
                }
                //wsdlDetail = ReadWriteTextFile.getContents(file);
            }
            setParameter(WSDL_NAME,wsdlName);
        }
    }
    
    public String getWsdlName() {
        return wsdlName;
    }
    
    public String getWsdlDetail() {
        return wsdlDetail;
    }
    
    public void actionListener(ActionEvent event) {
       logger.info("Action Listener called...");
    }
    
    private void reset() {
        policyGroupName = "";
        serviceGroupName = "";
        resetAlerts();
    }
    
     //
     // Getter for property wsdlUploadedFile.
     // @return Value of property wsdlUploadedFile.
     //
    public UploadedFile getWsdlUploadedFile() {
        return this.wsdlUploadedFile;
    }

     //
     // Setter for property wsdlUploadedFile.
     // @param wsdlUploadedFile New value of property wsdlUploadedFile.
     //
    public void setWsdlUploadedFile(UploadedFile uploadedFile) {
        logger.info(">>>> setUploadedFile called:" + uploadedFile);
        this.wsdlUploadedFile = uploadedFile;
        if (uploadedFile!=null ) {
            logger.info(">>>> setUploadedFile called: uploaded filename - "+uploadedFile.getOriginalName());
            wsdlFileName = uploadedFile.getOriginalName();
        }
    }
    
    public String getWsdlAction() {
        // @todo 
        logger.info("getWsdlAction...");
        if ( rbFolderSelected ) {
            logger.info("by file name..."+wsdlFileName);
            wsdlFileName = wsdlUploadedFile.getOriginalName();
            if ( wsdlFileName==null || wsdlFileName.length()<1 ) {
                logger.info("WSDL/XSD/Zip file is not specified.");
            } else {
               // if zip file - 
                if ( wsdlFileName.endsWith(AspectsGenericConstants.ZIP_SUFFIX) ) {
                    String outputFileName = saveFile();
                    // extract zip file...
                    logger.info("by zip file..."+outputFileName);
                    File zipfile = new File(outputFileName);
                    extractZipFile(zipfile,getWsdlServiceLocation());
                } else {
                    String outputFileName = saveFile();
                    addToWsdlList(wsdlFileName);
                }
            }        
        } else if ( rbUrlSelected ) {
            // @todo - get WSDL/XSD based on URL
            logger.info("by URL..."+wsdlUrl);
            if ( wsdlUrl!=null ) { 
                File folder = new File(getWsdlServiceLocation());
                extractWsdlFromUrl(wsdlUrl,folder);
            }
        }

        return "";
    }
    
    public boolean getUrlRadioButtonSelected() {
        logger.finest("get rbUrlSelected: "+rbUrlSelected);
        return rbUrlSelected;
    }

    public void setUrlRadioButtonSelected(boolean b) {
        rbUrlSelected = b;
        logger.finest("set rbUrlSelected: "+rbUrlSelected);
    }
    
    public void setFolderRadioButtonSelected(boolean b) {
        rbFolderSelected = b;
        logger.finest("set rbFolderSelected: "+rbFolderSelected);
    }
        
    public boolean getFolderRadioButtonSelected() {
        logger.finest("get rbFolderSelected: "+rbFolderSelected);        
        return rbFolderSelected;
    }
        
    private void setServiceFolderName() {
        // 
        if ( serviceFolderName==null || serviceFolderName.length()<1 ) {
            serviceFolderName = UUIDGenerator.getNUID();
        }
    }
    
    public boolean getDisableServiceGroupName() {
        return !serviceAdd;
    }
    
    /** Get the value of property alertDetail. */
    public String getAlertMessage() {
        return alertMessage;
    }
    
    public void setAlertMessage(String msg) {
        alertMessage = msg;
    }
    
    /** Get the value of property alertRendered. */
    public boolean getAlertMessageRendered() {
        return renderAlertMessage;
    }
    
    public void setRenderAlertMessage(boolean value) {
        renderAlertMessage = value;
    }

    /** return the value of alert summary     */
    public String getAlertSummary() {
        return alertSummary;
    }
    
    public void setAlertSummary(String summary) {
        alertSummary = summary;
    }
    
    /** return the value of alert type */
    public String getAlertType() {
        return alertType;
    }
    
    public void setAlertType(String type) {
        alertType = type;
    }    
    
    private void resetAlerts() {
        alertMessage = null;
        renderAlertMessage = false;
        setAlertType(ALERT_TYPE_ERROR);
        setAlertSummary(Messages.getString("configuration_alert_validation"));
    }
    
    public String validateService() {
        // todo
        // 1. check for duplicate service group name
        // 2. check for empty wsdl
        
        return "";
    }
    
    ///////////                                         /////////////
    ////////////////////// helper methods       /////////////////////
    ////////////                                        /////////////
    
    private String getWsdlFilesString() {
        StringBuffer list = new StringBuffer();
        for (Iterator iter=wsdlList.iterator(); iter.hasNext();) {
            DisplayWsdl wsdl = (DisplayWsdl)iter.next();
            if ( AspectsGenericConstants.WSDL_TYPE.equals(wsdl.getType()) ) {
                list.append(wsdl.getName());
                list.append(" ");
            }
        }
        return list.toString();
    }
    
    private String getXsdFilesString() {
        StringBuffer list = new StringBuffer();
        for (Iterator iter=wsdlList.iterator(); iter.hasNext();) {
            DisplayWsdl wsdl = (DisplayWsdl)iter.next();
            if ( AspectsGenericConstants.XSD_TYPE.equals(wsdl.getType()) ) {
                list.append(wsdl.getName());
                list.append(" ");
            }
        }
        return list.toString();
    }    
        
    private String getBaseLocation() {
        if ( baseLocation==null ) {
            ServletContext servletContext = getServletContext();
            String appPath = servletContext.getRealPath("/");
            File file = new File(appPath);
            File workspaceFolder = new File(appPath, "workspace");
            baseLocation = workspaceFolder.getAbsolutePath();
        }
        return baseLocation;
    }
    
    private String getCatalogLocation() {
        return getBaseLocation() + File.separator + "catalog";
    }
    
    private String getWsdlServiceLocation() {
        return getCatalogLocation() + File.separator + serviceFolderName;
    }
    
    private String getPolicyGroupsLocation() {
        return getBaseLocation() + File.separator + "aspectpolicygroups";
    }
    
    private String saveFile() {
        
        logger.info("saving file to workspace...");

        String installPath = getWsdlServiceLocation();
        File installFolder = new File(installPath);
        if (installFolder.exists() == false) {
             try {
                installFolder.mkdirs();
            } catch (RuntimeException e) {
                logger.log(Level.SEVERE,e.getMessage(),e);
            }
        }
        // write file to storage
        String outputFileName = null;
        try {
            File outputFile = new File(installFolder.getAbsolutePath()+File.separator+wsdlFileName);     
            outputFileName = outputFile.getCanonicalPath();
            logger.info("Saving "+outputFileName);
            wsdlUploadedFile.write(outputFile);
        } catch(Exception e) {
            e.printStackTrace();
            outputFileName = null;
        }
        
        return outputFileName;
    }    
    
    private void clearPolicyGroup() {
        policyGroupList.clear();
        policyGroupName = "";
    }
    
    private void getPolicyGroupsData() {
        String folderName = getPolicyGroupsLocation();
        File folder = new File(folderName);
        if (folder.isDirectory() == true) {
            File[] fileList = folder.listFiles();
            List<JBIServiceAssemblyStatus> saList = getServiceAssemblies();
            for (File f : fileList) {
                String name = f.getName();
                DisplayPolicyGroup policyGroupDisplay = new DisplayPolicyGroup(name);
                String status = getServiceAssemblyStatus(name,saList);                    
                policyGroupDisplay.setStatus(status);
                policyGroupList.add(policyGroupDisplay);
            }
        }        
    }
    
    private void clearPolicyGroups() {
        policyGroupList.clear();
    }
    
    private void clearServices() {
        serviceList.clear();
        serviceGroupName = null;
        serviceFolderName = null;
    }
    
    private void clearWsdls() {
        wsdlGroup = null;
        wsdlList.clear();
        serviceFolderName = null;
    }
    
    private void saveToCatalog() {
        List<WSDLService> serviceList = null;
        File workspaceFolder = new File(getBaseLocation());
        
        // @todo - case for add/edit
        try {
            String folderName = getWsdlServiceLocation();
            File folder = new File(folderName);
            if(folder.exists() == false) {
                folder = FileUtilities.createFolder(folder.getAbsolutePath());
            }
            File[] fileList = null;
            if (folder.isDirectory() == true) {
                fileList = folder.listFiles();
            }
            if ( serviceAdd ) {
                // for adds - add to catalog
                serviceList = Catalog.createServiceEntries(serviceGroupName, workspaceFolder, folder, fileList, false);
            } else {
                // for edits - update Catalog
                catalog.updateServiceEntries(serviceGroupName,workspaceFolder,folder,fileList);
            }
            
        } catch(Exception e) {
            e.printStackTrace();
        }
    }
    
    private Catalog getCatalog() {
        return CatalogReader.getCatalog(getBaseLocation()+File.separator+AspectsGenericConstants.CATALOG_XML);
    }
    
    private String getWebServicesString() {
        Catalog catalog = getCatalog();
        return catalog.constructInputFromCatalog(catalog);
    }

    private List<JBIServiceAssemblyStatus> getServiceAssemblies() {

        ServiceUnitsBean suBean = new ServiceUnitsBean();
        // @todo - set the targetName
        List<JBIServiceAssemblyStatus> saList = suBean.getSAList(GenericConstants.ADMIN_SERVER);
        return saList;
    }
        
    private String getServiceAssemblyStatus(String pgName, List<JBIServiceAssemblyStatus> saList) {
        String saStatus = GenericConstants.UNKNOWN_STATE;
        for ( Iterator iter=saList.iterator(); iter.hasNext(); ) {
            JBIServiceAssemblyStatus sa = (JBIServiceAssemblyStatus)iter.next();
            String name = sa.getServiceAssemblyName();
            String desc = sa.getServiceAssemblyDescription();
            String status = sa.getStatus();
            if ( name.equalsIgnoreCase(pgName) ) {
                saStatus = status;
                break;
            }
        }
        return saStatus;
    }
    
    private void getWsdlsFromCatalog() {
        Catalog catalog = getCatalog();
        if ( catalog!=null ) {
            List<WSDLService> wsdlServiceList = catalog.getWSDLServiceList(catalog,serviceGroupName);
            if ( wsdlServiceList!=null ) {
                // get wsdl files 
                for ( WSDLService wsdlService : wsdlServiceList ) {
                    // set service folder name
                    if ( serviceFolderName==null ) {
                        serviceFolderName = wsdlService.getFolder().getName();
                    }
                    // get service wsdl names
                    List<File> wsdls = wsdlService.getWsdlFiles();
                    for ( File wsdl : wsdls ) {
                        String wsdlName = wsdl.getName();
                        DisplayWsdl displayWsdl = new DisplayWsdl(wsdlName);
                        if ( wsdlList.contains(displayWsdl)==false ) {
                            wsdlList.add(displayWsdl);
                        }
                    }
                    // get xsd files
                    List<File> xsds = wsdlService.getXsdFiles();
                    for ( File xsd : xsds ) {
                        DisplayWsdl displayWsdl = new DisplayWsdl(xsd.getName());
                        if ( wsdlList.contains(displayWsdl)==false ) {
                            wsdlList.add(displayWsdl);
                        }
                    }
                }
            }
        }
    }
    
    private void getServicesFromCatalog() {
        /// check if list is empty
        if ( serviceList.isEmpty() ) {
            Catalog catalog = getCatalog();
                if ( catalog!=null ) {
                    List<String> serviceGroups = catalog.getServiceGroupList(catalog);
                    if ( serviceGroups!=null ) {
                        for ( String serviceGroup : serviceGroups ) {
                            DisplayService displayService = new DisplayService(serviceGroup);
                            serviceList.add(displayService);
                        }
                    }
            }
            
        }
    }
    
    
    private void addToWsdlList(String fileName) {
        // add wsdlFile name to list
        DisplayWsdl displayWsdl = new DisplayWsdl(fileName);
        wsdlList.add(displayWsdl);
    }
    
    
    private void extractZipFile(File wsdlxsdZipFile, String folder) {
        List<ZipEntry> entries = ZipFileUtility.listFileContents(wsdlxsdZipFile.getAbsolutePath());
        for (ZipEntry entry : entries) {
            String name = entry.getName();
            File file = new File(name);
            if (name.endsWith(AspectsGenericConstants.WSDL_SUFFIX) == true || 
                name.endsWith(AspectsGenericConstants.XSD_SUFFIX) == true )  {
                // store file to disk
                File aFile = ZipFileUtility.readFileFromArchive(wsdlxsdZipFile
                        .getAbsolutePath(), name, folder + File.separator + file.getName());
                addToWsdlList(file.getName());
            }
        }  
    }
    
    private void extractWsdlFromUrl(String url, File folder) {
        WSDLModel wsdlModel = new WSDLModelHelper();
        String rawXmlData = wsdlModel.getWSDLString(url);
        String uniqueID = UUIDGenerator.getNUID();
        Catalog.createWorkspaceFolder(folder);
        File wsdlFile = new File(folder, uniqueID + AspectsGenericConstants.WSDL_SUFFIX );
        try {
            FileUtilities.writeToFile(wsdlFile, rawXmlData);
            addToWsdlList(wsdlFile.getName());
        } catch(Exception e) {
            e.printStackTrace();
        }
    }

    
    private void setHostname() {
        String hostName = (String)getParameter(AspectsGenericConstants.HOST_NAME);
        if ( hostName==null ) {
            try {
                hostName = InetAddress.getLocalHost().getCanonicalHostName();
                setParameter(AspectsGenericConstants.HOST_NAME,hostName);
            } catch(Exception e) {
                // 
                e.printStackTrace();
            }
        }
        logger.info("hostName: "+hostName);
    }
    
    
    private void delete(RowKey[] rowKeys, Group group) {
        delete(rowKeys,group,"name");
    }
    
    // Action to remove rows from ObjectListDataProvider.
    private void delete(RowKey[] rowKeys, Group group, String fieldKeyName) {
        logger.info("### delete: rowkeys="+rowKeys);
        Select select = group.getSelect();
        TableDataProvider groupProvider = group.getData();
        if (rowKeys==null || group==null || groupProvider==null || select==null ) {
            logger.info("** RowKeys, Group, Provider, or Select object(s) is <null>. Delete is aborted! **");
            return;
        }
        for (int i = 0; i < rowKeys.length; i++) {
            RowKey rowKey = rowKeys[i];
            if (groupProvider.canRemoveRow(rowKey)) {
                logger.info("## removing rowkey:"+rowKey);
                // print out some info
                if ( fieldKeyName!=null ) {
                    //String name= (String)groupProvider.getValue(groupProvider.getFieldKey(fieldKeyName),rowKey);
                    //logger.info("## name: "+name);
                    boolean selected = select.getSelectedState(rowKey);
                    logger.info("## selected: "+selected);
                }
                groupProvider.removeRow(rowKey);
                // remove file from filesystem
                removeFile(rowKey.getRowId());
            }
        }
        ((ObjectListDataProvider) groupProvider).commitChanges(); // Commit.
        group.getSelect().clear(); // Clear phase listener.
        
    }        
    
    
    private void removeFile(String rowId) {
        logger.info("removing file with rowId:"+rowId);
        try {
            DisplayWsdl displayWsdl = wsdlList.get(Integer.parseInt(rowId));
            String filename = displayWsdl.getName();
            logger.info("removing file: "+filename);
            // now remove the file from filsystem
            File file = new File(getWsdlServiceLocation(),filename);
            file.delete();
        } catch(Exception e) {
            e.printStackTrace();
        }
    }
    
    
}
