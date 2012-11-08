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
 * @(#)BPELModelProcessor.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

 package com.sun.jbi.cam.plugins.bpelse;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.sql.Blob;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.jar.JarEntry;
import java.util.jar.JarInputStream;
import java.util.logging.Logger;

import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELDocumentParseFactory;
import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.DefaultWSDLResolverFactory;
import com.sun.bpel.model.DefaultXSDResolverFactory;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RVariable;
import com.sun.bpel.model.parser.impl.ParseContextImpl;
import com.sun.bpel.model.visitor.IWSDLResolver;
import com.sun.bpel.model.visitor.IXSDResolver;
import com.sun.jbi.cam.plugins.bpelse.bpvisualizer.BPCustomChartBean;
import com.sun.jbi.cam.plugins.bpelse.bpvisualizer.BPELProcessMaps;
import com.sun.jbi.cam.plugins.bpelse.bpvisualizer.BPCustomChartBean.VAR_TYPE;
import com.sun.jbi.cam.plugins.bpelse.common.BpelsePluginMessages;

public class BPELModelProcessor {

    public static int BUFFER_SIZE = 4096;
    private final static String GET_BPEL_PATH_QUERY =
        "select SUZIPARCHIVE from SERVICEUNIT  where SUNAME=" +
               " 'suName' "; 
    private final static String SU_ARCHIVE = "SUZIPARCHIVE";
    private Logger logger = Logger.getLogger(BPCustomChartBean.class.getName());
    private static BPELModelProcessor processor;
    private DBManager dbManager;
    private Map<String,List<String>> su2BpelsIdMap;
    private Map<String,BPELProcessMaps> suVariblesMap;
    
    private BPELModelProcessor(){
        this.dbManager = new DBManager();
        su2BpelsIdMap = new HashMap<String,List<String>>();
        suVariblesMap = new HashMap<String,BPELProcessMaps>();
    }
    
    public static synchronized BPELModelProcessor getInstance() {
        if(processor == null) {
            processor = new BPELModelProcessor();
        }
        return processor;
    }

    public void setDBManager( DBManager dbManager) {
        this.dbManager =  dbManager;
    }
    
    /**
     * return a list of bpelid associated with the provided service unit.
     * As a side affect it will generate variables map if not proccessed yet for
     * this service unit.
     * @param suName - the name of the service unit
     * @return a list contain 1 or more bpel id. the bpel id is QName extracted
     *         from the bpel model. it is converted to string value.
     * @throws Exception if query to the persistence database had error.
     */
    public  List<String> getBPELIDList(String suName) throws Exception {
        
        if(su2BpelsIdMap.containsKey(suName)) {
            return su2BpelsIdMap.get(suName);
        }

        List <BPELProcessWrapper> BPELProcessList = getBPELProcessList(suName);
        // create variables maps if need;
        createVariablesMaps(suName);

        List<String> bpelIdList = getBPELIDList(suName,BPELProcessList);
        return bpelIdList;
    }
    
    
    /**
     * populate the varibales maps provided as parameters to this method.
     * As a side affect it will generate bpelid list if not proccessed yet for
     * this service unit.
     * @param suName - the name of the service unit.
     * @return  BPELProcessMaps object {@link} 
     * @param bpelName2id2TypeMap instance or null in case of exception
     * @throws Exception
     */
    public  BPELProcessMaps createVariablesMaps(String suName) throws Exception {
        logger.finest("BPELModelProcessor.createVariablesMaps() for su= " + suName);

        if(suVariblesMap.containsKey(suName)) {
            // the su already processed return cached values
            BPELProcessMaps bpMaps = suVariblesMap.get(suName);
            logger.finest("bpMaps= " + bpMaps);
            logger.finest("bpMaps.getBpelName2id2TypeMap = " + bpMaps.getBpelName2id2TypeMap().keySet().toArray());
            logger.finest("bpMaps.getBpelProcessesMap = " + bpMaps.getBpelProcessesMap());
            
            return bpMaps;
        }
       
        List<BPELProcessWrapper> processes;

        try {
            processes = getBPELProcessList(suName);
        } catch (Exception ex) {
            logger.warning(ex.getMessage());
            return null;
        }
        // create bpelid list if needed
        getBPELIDList(suName,processes);
        
        
        
        BPELProcessMaps bpMaps = createVariablesMaps(suName,processes);
        return  bpMaps;
    }
    
    private BPELProcessMaps createVariablesMaps(String suName,List<BPELProcessWrapper> processes) {
        logger.finest("BPELModelProcessor.createVariablesMaps() for su= " + suName 
                + " processes = " + processes);
        Map<String,List<Map<String,String>>>bpelProcessesMap =
            new HashMap<String,List<Map<String,String>>>();
        Map<String,Map<String,VAR_TYPE>> bpelName2id2TypeMap = 
            new HashMap<String,Map<String,VAR_TYPE>>();
        
        BPELProcessMaps bpmaps = new BPELProcessMaps();
        
        for (BPELProcessWrapper processWrapper : processes) {
            boolean hasSimpleVars = false;
            Collection<RVariable> variablesColl =
                processWrapper.getBpelProcess().getVariables().getVariables();
            if(variablesColl.size() == 0) {
                continue;
            }
            // set up an bpel process entry in the map
            List<Map<String,String>> varsIdsMapList = new ArrayList<Map<String,String>>();
            Map<String,String> variable2IdMap = new HashMap<String,String>();
            Map<String,String> id2VariableMap = new HashMap<String,String>();
            Map<String,VAR_TYPE> id2VariableTypeMap = new HashMap<String,VAR_TYPE>();
            varsIdsMapList.add(variable2IdMap);
            varsIdsMapList.add(id2VariableMap);
            // build the vars/id maps
            for (RVariable  bpelVar : variablesColl) {
                
                // skip non trivial variables  -
                // disable until varsimple table has real data
                if(bpelVar.isBoolean() || bpelVar.isNumber() ||
                      bpelVar.isString() || bpelVar.isDate()) {
                hasSimpleVars = true;    
                // in order to insure uniqueness of the variable name
                // in the case where the composite application has
                // multiple bpel we prepend the bpel name to the
                // varible name
                
                String varName = processWrapper.getBpelName()+ "." +
                        bpelVar.getName();

                String  varUniqueId =  processWrapper.getBpelId()+ "." +
                        bpelVar.getUniqueId();
                logger.finest(suName +  " has var named=" + varName + " with id=" + varUniqueId);
                variable2IdMap.put(varName,varUniqueId);
                id2VariableMap.put(varUniqueId,varName);
                if(bpelVar.isBoolean()) {
                    id2VariableTypeMap.put(varUniqueId,VAR_TYPE.BOOLEAN);
                    logger.finest(" the var type is boolean");
                } else if(bpelVar.isNumber()) {
                    id2VariableTypeMap.put(varUniqueId,VAR_TYPE.NUMERIC);
                    logger.finest(" the var type is numeric");
                } else if (bpelVar.isString()) {
                    id2VariableTypeMap.put(varUniqueId,VAR_TYPE.STRING);
                    logger.finest(" the var type is string");
                } else if (bpelVar.isDate()) {
                    id2VariableTypeMap.put(varUniqueId,VAR_TYPE.DATETIME);
                    logger.finest(" the var type is date");
                }
              }
                
            }
            if(hasSimpleVars) {            
                bpelProcessesMap.put(processWrapper.getBpelName(),varsIdsMapList);
                bpelName2id2TypeMap.put(processWrapper.getBpelName(), id2VariableTypeMap);
            }
        }
        logger.finest("BPELModelProcessor.createVariablesMaps() for su= " + suName 
                + " bpmaps = " + bpmaps);
        bpmaps.setBpelName2id2TypeMap(bpelName2id2TypeMap);
        logger.finest("BPELModelProcessor.createVariablesMaps() for su= " + suName 
                + " adding bpelName2id2TypeMap =" + bpelName2id2TypeMap + "to bpmaps");
        bpmaps.setBpelProcessesMap(bpelProcessesMap);
        logger.finest("BPELModelProcessor.createVariablesMaps() for su= " + suName 
                + " adding bpelProcessesMap =" + bpelProcessesMap + "to bpmaps");
        suVariblesMap.put(suName, bpmaps);
        return bpmaps;
    }

    
    private  List<String> getBPELIDList(String suName,List<BPELProcessWrapper> BPELProcessList) throws Exception {
        
        List<String> bpelIdList = new ArrayList<String>();
        for (BPELProcessWrapper wrapper : BPELProcessList) {
            String bpelId = wrapper.getBpelId();
            bpelIdList.add(bpelId);
        }
        su2BpelsIdMap.put(suName, bpelIdList);
        return bpelIdList;
    }

    
    private List <BPELProcessWrapper> getBPELProcessList(String suName) throws Exception {
        InputStream    bpelArchive = getBPELsArchive(suName);
        if(bpelArchive == null) {
            throw new Exception(BpelsePluginMessages.getString("bpvisualizer_custom_no_for_su")+
                    suName);
        }
        
        List <BPELProcessWrapper> BPELProcessList = getBPELsProcesses(bpelArchive,
                suName);
       return BPELProcessList;
    }
    
    /*
     * the method return the the service unit from the serviceUnit table. 
     * this stream contain a jar which holds the bpel file/s associated
     * with this service unit.
     * 
     * @parameter suName - the name of the service unit
     * @return inputstream reprenting the acrhive or null if archhive
     *  not found or sql error.
     */
    private InputStream  getBPELsArchive(String suName) throws Exception{
       InputStream archiveBytes = null;
       String query = GET_BPEL_PATH_QUERY.replace("suName", suName);
       ResultSet resultset = dbManager.execGenericQuery(query);
       
       if(resultset == null) {
           return archiveBytes;
       }
        try {
            
            while (resultset.next() ){
                 
                Blob suArchive = resultset.getBlob(SU_ARCHIVE);
                return suArchive.getBinaryStream();
            }
        } catch (SQLException ex) {
            ex.printStackTrace();
            throw ex;
       }finally {
           dbManager.closeGenericConnnection();
       }
       return archiveBytes;
        
    }
    
    private  List <BPELProcessWrapper> getBPELsProcesses(InputStream bpelJarIS,
            String suName) throws Exception{
        List <BPELProcessWrapper>  BPELProcessList =
                new ArrayList<BPELProcessWrapper>();
        Map<String,String> bpelsMap = new HashMap<String,String>(); 

        JarInputStream bpelsJarFile = new JarInputStream(bpelJarIS);
        // walk its entries
        JarEntry bpelWsdlEntry = bpelsJarFile.getNextJarEntry();
        // get the bpels jar entry
        while (bpelWsdlEntry != null) {
//            String entryName = bpelWsdlEntry.getName();
            // extract all files from the bpel service unit
            extractBpelOrWsdlFile(bpelsMap,bpelsJarFile,bpelWsdlEntry);
            bpelWsdlEntry = bpelsJarFile.getNextJarEntry();
        }
        // use the map to get the bpelProcess
        for (String bpelRelativepath : bpelsMap.keySet()) {
            if(!bpelRelativepath.endsWith(".bpel")) {
                // skip wsdl entries
                 continue;
            }
            String bpelfilepath  = bpelsMap.get(bpelRelativepath);
            File bpelFile = new File(bpelfilepath);
            String bpelFileURI = bpelFile.toURI().toString();
            // try {
            URL url = new URL("file", null, bpelfilepath); // NO I18N at present
            InputStream is = url.openStream();
            InputStreamReader reader = new InputStreamReader(is, "UTF-8");

            BPELParseContext parseContext = new ParseContextImpl();

            parseContext.setCatalog(bpelFile);
            IWSDLResolver wsdlResolver =
                    DefaultWSDLResolverFactory.getInstance().newWSDLResolver(
                    bpelFileURI, parseContext);
            parseContext.setWSDLResolver(wsdlResolver);

            // set the xsd resolver
            IXSDResolver xsdLoader =
                    DefaultXSDResolverFactory.getInstance().newXSDResolver(
                    bpelFileURI, parseContext);
            parseContext.setXSDResolver(xsdLoader);

            BPELDocument bpelDoc = BPELDocumentParseFactory.getInstance().load(reader,
                    parseContext);
            bpelDoc.setBaseURI(bpelfilepath);

            RBPELProcess bProc = (RBPELProcess) bpelDoc.getDocumentProcess();
            BPELProcessWrapper wrapper = 
                    new BPELProcessWrapper(bProc);
            BPELProcessList.add(wrapper);
            
        }
        // clean up temprary files
        for (String bpelRelativepath : bpelsMap.keySet()) { 
           File tempFile = new File(bpelsMap.get(bpelRelativepath));
           tempFile.delete();
        }
        
        
        return BPELProcessList;
    }
    
    private void extractBpelOrWsdlFile( Map<String,String> bpelWsdlMap,
            JarInputStream bpelsJarFile,JarEntry entry) throws Exception  {
        String entryName = entry.getName().substring(1);
        String tempFilePath = 
                   System.getProperty("java.io.tmpdir") + entryName;
        createTempDirIfNeeded(tempFilePath);
        byte[] entryBytes = null;
        File tempFile = new File(tempFilePath);
        FileOutputStream tempFileOS = new FileOutputStream(tempFile);
        boolean sizeKnown = entry.getSize() != -1;
        if(sizeKnown) {
          entryBytes = new byte[(int)entry.getSize()]; 
          bpelsJarFile.read(entryBytes);
          tempFileOS.write(entryBytes);
        } else {
          entryBytes = new byte[BUFFER_SIZE]; 
          int bytesRead = bpelsJarFile.read(entryBytes);
          while (true) {
              tempFileOS.write(entryBytes,0,bytesRead);
              tempFileOS.flush();
              bytesRead = bpelsJarFile.read(entryBytes);
              if(bytesRead==-1){
                  break;
              }
          }
        }
        tempFileOS.close();
        bpelWsdlMap.put(entryName, tempFilePath);

    }
    
    private void createTempDirIfNeeded(String filePath) throws Exception{
        String tempDir = System.getProperty("java.io.tmpdir");
        int fileNamePrefixIndex = filePath.lastIndexOf("/");
        if(fileNamePrefixIndex == -1) {
            return;
        }
        
        if(tempDir.equals(filePath.substring(0,fileNamePrefixIndex-1))) {
            return; // file does not have relative path
        }
        
        File tempFile = new File(filePath.substring(0,fileNamePrefixIndex));
        if(tempFile.exists()) {
            return;
        }
        tempFile.mkdirs();
         
   }
    
   class BPELProcessWrapper {
        private String bpelName;
        private RBPELProcess bpelProcess;
        
        
        private BPELProcessWrapper(RBPELProcess bpelProcess) {
            setBpelProcess(bpelProcess);
        }
        
        private String getBpelName() {
            return bpelProcess.getBPELId().getLocalPart();
        }
        
        private RBPELProcess getBpelProcess() {
            return bpelProcess;
        }
        private void setBpelProcess(RBPELProcess bpelProcess) {
            this.bpelProcess = bpelProcess;
        }
        
        private String getBpelId() {
            return bpelProcess.getBPELId().toString();
        }
        
   }
}
