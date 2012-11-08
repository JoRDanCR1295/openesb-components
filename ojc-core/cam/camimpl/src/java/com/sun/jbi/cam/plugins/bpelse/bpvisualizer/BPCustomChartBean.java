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
 * @(#)BPCustomChartBean.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse.bpvisualizer;

import com.sun.jbi.cam.common.resources.Messages;
import com.sun.bpel.model.BPELDocument;
import com.sun.bpel.model.BPELDocumentParseFactory;
import com.sun.bpel.model.BPELParseContext;
import com.sun.bpel.model.DefaultWSDLResolverFactory;
import com.sun.bpel.model.DefaultXSDResolverFactory;
import com.sun.bpel.model.visitor.IWSDLResolver;
import com.sun.bpel.model.visitor.IXSDResolver;
import com.sun.bpel.model.parser.impl.ParseContextImpl;
import com.sun.bpel.model.meta.RBPELProcess;
import com.sun.bpel.model.meta.RVariable;
import com.sun.webui.jsf.component.DropDown;
import com.sun.webui.jsf.model.Option;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Logger;
import javax.faces.context.FacesContext;
import javax.faces.event.ActionEvent;
import org.apache.commons.validator.routines.DoubleValidator;
import org.apache.commons.validator.routines.IntegerValidator;
import org.apache.commons.validator.routines.LongValidator;
import org.apache.commons.validator.routines.FloatValidator;
import org.apache.commons.validator.routines.DateValidator;


/**
 *
 * @author rdamir
 */
public class BPCustomChartBean extends ChartingConstants{
    
    private Logger logger = Logger.getLogger(BPCustomChartBean.class.getName());
    public static String BEAN_NAME = "#{CustomChartBean}";
  
    private BPVisualizerDBHelper dbHelper;
    private String selectedXVariable;
    private String selectedYVariable;
    private BPVisualizerBean visualizerBean;
    private String currentServiceUnit= null;
    private Map<String,String> variable2IdMap;
    private Map<String,String> id2VariableMap;
    
    public enum VAR_TYPE { BOOLEAN, NUMERIC ,STRING,DATETIME};
    private Map<String,VAR_TYPE> id2VariableTypeMap;
    private VAR_TYPE selectedXVariableType;
    private VAR_TYPE selectedYVariableType;
    
    private String lowerLimit_x_axis;
    private String upperLimit_x_axis;
    private String lowerLimit_y_axis;
    private String upperLimit_y_axis;
    
    private Date dateLowerLimit_x_axis;
    private Date dateUpperLimit_x_axis;
    private Date dateLowerLimit_y_axis;
    private Date dateUpperLimit_y_axis;
    
    private boolean xVariableLimitsDisabled;
    private boolean yVariableLimitsDisabled;
    private boolean customPlotRequested;
    private boolean hasValidEntries;

    private StringBuffer validationErrorBuffer;
    
    /** Creates a new instance of BPCustomChartSupport */
    public BPCustomChartBean() {
         clearCustomSelection();
        dbHelper = new BPVisualizerDBHelper();
        javax.faces.context.FacesContext context =
                javax.faces.context.FacesContext.getCurrentInstance();
        
        javax.faces.el.ValueBinding valueBinding =
                context.getApplication().createValueBinding(BPVisualizerBean.BEAN_NAME);
        
        visualizerBean =
                (BPVisualizerBean) valueBinding.getValue(context);
    }
    
    public boolean getXVariableLimitsDisabled() {
        xVariableLimitsDisabled = isXVariableHasLimit()==true?false:true; 
        return xVariableLimitsDisabled;
    }
    
     public boolean getYVariableLimitsDisabled() {
        yVariableLimitsDisabled = isYVariableHasLimit()==true?false:true;
        return yVariableLimitsDisabled;
  }
   
    public VAR_TYPE getCurrentXVaribleType() {
        return selectedXVariableType;
    }

    public VAR_TYPE getCurrentYVaribleType() {
        return selectedYVariableType;
    }
    
    public String getLowerLimit_X_Axis() {
        return lowerLimit_x_axis == null ? "" : lowerLimit_x_axis;
    }

    public void setLowerLimit_X_Axis(String limit) {
       lowerLimit_x_axis = limit;
    }
    

    public String getUpperLimit_X_Axis() {
        return upperLimit_x_axis == null ? "" : upperLimit_x_axis;
    }

    public void setUpperLimit_X_Axis(String limit) {
       upperLimit_x_axis = limit;
       
    }

    public String getLowerLimit_Y_Axis() {
        return lowerLimit_y_axis == null ? "" : lowerLimit_y_axis;
    }

    public void setLowerLimit_Y_Axis(String limit) {
       lowerLimit_y_axis = limit;
       
    }
    

    public String getUpperLimit_Y_Axis() {
        return upperLimit_y_axis == null ? "" : upperLimit_y_axis;
    }

    public void setUpperLimit_Y_Axis(String limit) {
       upperLimit_y_axis = limit;
       
    }

    public Date getDateLowerLimit_X_Axis() {
        return dateLowerLimit_x_axis;
    }

    public void setDateLowerLimit_X_Axis(Date limit) {
       dateLowerLimit_x_axis = limit;
    }
    

    public Date getDateUpperLimit_X_Axis() {
        return dateUpperLimit_x_axis;
    }

    public void setDateUpperLimit_X_Axis(Date limit) {
       dateUpperLimit_x_axis = limit;
       
    }

    public Date getDateLowerLimit_Y_Axis() {
        return dateLowerLimit_y_axis;
    }

    public void setDateLowerLimit_Y_Axis(Date limit) {
       dateLowerLimit_y_axis = limit;
       
    }
    

    public Date getDateUpperLimit_Y_Axis() {
        return dateUpperLimit_y_axis;
    }

    public void setDateUpperLimit_Y_Axis(Date limit) {
       dateUpperLimit_y_axis = limit;
    }

    
    
    public boolean hasSimpleVars() {
        return variable2IdMap != null && variable2IdMap.size() > 0;
    }
    
    public String getAxis_X_SelectedOption() {
        return selectedXVariable;
    }
    
    public String getAxis_Y_SelectedOption() {
        return selectedYVariable;
    }
 
    public void setAxis_X_SelectedOption(String selected) {
         selectedXVariable = selected;
         if(id2VariableTypeMap!= null) {
           selectedXVariableType = id2VariableTypeMap.get(selected);
         }
    }
    
    public void setAxis_Y_SelectedOption(String selected) {
        selectedYVariable= selected;
         if(id2VariableTypeMap!= null) {
           selectedYVariableType = id2VariableTypeMap.get(selected);
         }
    }

    
    public Option[] getAxis_X_SelectionMenuOptions() throws Exception {
        checkServiceUnitChanged();
        Map<String,String> tempVariable2IdMap= new HashMap<String,String>();
       // filter out selection if any
        for (String varName : variable2IdMap.keySet()) {
            String id = variable2IdMap.get(varName);
             // remove the selected y variable from x variables list
            if(id.equals(selectedYVariable)) {
                continue;
            }
            // the folllowing is the logic that implement the 
            // remove of invalid selection of x value type based on 
            // the y value type
//            VAR_TYPE xType = id2VariableTypeMap.get(id);
//            if(xType != VAR_TYPE.NUMERIC && 
//                    getCurrentYVaribleType() != VAR_TYPE.NUMERIC) {
//                continue;
//            }
            tempVariable2IdMap.put(varName,id);
        }
        
        Option[] varsOptions = buildOptionList(tempVariable2IdMap);
        return varsOptions;
        
    }
    
    public Option[] getAxis_Y_SelectionMenuOptions() throws Exception {
        checkServiceUnitChanged();
        Map<String,String> tempVariable2IdMap= new HashMap<String,String>();
       // filter out selection if any
        for (String varName : variable2IdMap.keySet()) {
            String id = variable2IdMap.get(varName);
             // remove the selected x variable from y variables list
            if(id.equals(selectedXVariable)) {
                continue;
            }
            // the folllowing is the logic that implement the 
            // remove of invalid selection of y value type based on 
            // the x value type
//            VAR_TYPE yType = id2VariableTypeMap.get(id);
//            if(yType != VAR_TYPE.NUMERIC && 
//                    getCurrentXVaribleType() != VAR_TYPE.NUMERIC) {
//                continue;
//            }
            tempVariable2IdMap.put(varName,id);
        }
        
        Option[] varsOptions = buildOptionList(tempVariable2IdMap);
        return varsOptions;
        
    }
    
    
    public void process_X_AxisSelectionMenuSelection(ActionEvent actionEvent) {
        // Since the action is immediate, the components won't go through the
        // Update Model phase.  So, we need to explicitly set the values and
        // update the model object for the given action selected.
        FacesContext context = FacesContext.getCurrentInstance();
        DropDown dropDown = (DropDown) actionEvent.getComponent();
        String selected = (String) dropDown.getSelected();
        this.setAxis_X_SelectedOption(selected);
    }
    
    public void process_Y_AxisSelectionMenuSelection(ActionEvent actionEvent) {
        // Since the action is immediate, the components won't go through the
        // Update Model phase.  So, we need to explicitly set the values and
        // update the model object for the given action selected.
        FacesContext context = FacesContext.getCurrentInstance();
        DropDown dropDown = (DropDown) actionEvent.getComponent();
        String selected = (String) dropDown.getSelected();
        this.setAxis_Y_SelectedOption(selected);
     }
    
    
    public void clearCustomSelection() {
         selectedXVariable = null;
         selectedYVariable = null;
         selectedXVariableType = VAR_TYPE.STRING;
         selectedYVariableType = VAR_TYPE.STRING;
         hasValidEntries = true;
         validationErrorBuffer = null;
       
    }
    
    public boolean hasValidData(){
        return hasValidEntries;
    }
    
    
    public boolean wasCutomPlotRequested() {
        return customPlotRequested;
    }
    
    public void plotCustomChart() {     
        validationErrorBuffer = new StringBuffer();
       // Validate selection prior to setting the 
        // customPlotRequested to true.
        ValidateEntries();
        if(hasValidEntries == true) {
        
            customPlotRequested = true;
        } else {
            customPlotRequested = false;
        }
    }

    
    
    public void checkServiceUnitChanged() throws Exception {
        boolean sameUnit = true;
        if(currentServiceUnit == null)  {
            sameUnit = false;
        } else {
            sameUnit = currentServiceUnit.equals(visualizerBean.getServiceUnitInstanceSelected());
        }
        // query db only on start up or su changed
        if(sameUnit == false) {
            currentServiceUnit = visualizerBean.getServiceUnitInstanceSelected();
            clearCustomSelection();
            // refresh variable map
            createVariablesMaps(currentServiceUnit);
        }
        
    }
    
    public String getValidationErrorMessage() {
        return validationErrorBuffer.toString();
    }
    
    
    private Option[] buildOptionList(Map<String,String> map) {
         int index = 0;
         Option[] varsOptions = new Option[1+map.size()];
         varsOptions[index++] = new Option(SELECT_VAR);
         for (String varName : map.keySet()) {
            String id = variable2IdMap.get(varName);
            varsOptions[index++] = new Option(id,varName);
        }
        return varsOptions;
    }

    public boolean isXVariableHasLimit() {
        if((selectedXVariableType == VAR_TYPE.NUMERIC) ||
           (selectedXVariableType == VAR_TYPE.DATETIME)) {
            return true;
        }     
        return false;
    }
    
    public boolean isYVariableHasLimit() {
        if((selectedYVariableType == VAR_TYPE.NUMERIC) ||
           (selectedYVariableType == VAR_TYPE.DATETIME)) {
            return true;
        }     
        return false;
    }
    

    
    private  void createVariablesMaps(String suName) throws Exception {
        if(variable2IdMap == null) {
            variable2IdMap =  new HashMap<String,String>();
            id2VariableMap =  new HashMap<String,String>();
            id2VariableTypeMap = new HashMap<String,VAR_TYPE>();
        } else {
            variable2IdMap.clear();
            id2VariableMap.clear();
            id2VariableTypeMap.clear();
        }
        List<BPELProcessWrapper> processes;
        try {
            processes = getBPELProcess(suName);
        } catch (Exception ex) {
            logger.warning(ex.getMessage());
            return;
        }
        for (BPELProcessWrapper processWrapper : processes) {
            Collection<RVariable> variablesColl = 
                    processWrapper.getBpelProcess().getVariables().getVariables();
            for (RVariable  bpelVar : variablesColl) {
                
              // skip non trivial variables  - 
              // disable until varsimple table has real data  
   //           if(bpelVar.isBoolean() || bpelVar.isNumber() || 
   //                bpelVar.isString() || bpelVar.isDate()) {// need to add datetime
                // in order to insure uniqueness of the variable name
                // in the case where the composite application has
                // multiple bpel we prepend the bpel name to the 
                // varible name
                String varName = processWrapper.getBpelName()+ "." + 
                        bpelVar.getName();
                String  varUniqueId =  processWrapper.getBpelName()+ "." + 
                            bpelVar.getUniqueId();
                variable2IdMap.put(varName,varUniqueId);
                id2VariableMap.put(varUniqueId,varName);
                if(bpelVar.isBoolean()) {
                    id2VariableTypeMap.put(varUniqueId,VAR_TYPE.BOOLEAN);
                } else if(bpelVar.isNumber()) {
                    id2VariableTypeMap.put(varUniqueId,VAR_TYPE.NUMERIC);
                } else if (bpelVar.isString()) {
                    id2VariableTypeMap.put(varUniqueId,VAR_TYPE.STRING);
                } else {
                    // assumed datetime until bpelse provides support for this
                    // data type.
                    id2VariableTypeMap.put(varUniqueId,VAR_TYPE.DATETIME);
                }
  //          }

           }
        }
    }
    
    private  List<BPELProcessWrapper> getBPELProcess(String suName) throws Exception {
        List<String>    bpelsfilepath = dbHelper.getBPELPath(suName);
        if(bpelsfilepath.size()==0) {
           throw new Exception(Messages.getString("bpvisualizer_custom_no_for_su")+
                        suName);
        }
        List <BPELProcessWrapper> BPELProcessList = 
                new ArrayList<BPELProcessWrapper>();
        for (String bpelfilepath : bpelsfilepath) {
            
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
                    new BPELProcessWrapper(bpelfilepath,bProc);
            BPELProcessList.add(wrapper);
        }
        return BPELProcessList;
    }
    
    private void ValidateEntries() {
        
      
        hasValidEntries = isVariablesSelected();
        if(!hasValidEntries) {
            return;
        }
        if(selectedXVariableType == VAR_TYPE.NUMERIC) {
            validateNumericEntry(true);
        } else  if(selectedXVariableType == VAR_TYPE.DATETIME) {
            validateDateEntry(true);
        }
        
        if(selectedYVariableType == VAR_TYPE.NUMERIC) {
            validateNumericEntry(false);
        } else  if(selectedYVariableType == VAR_TYPE.DATETIME) {
            validateDateEntry(false);
        }
        
    }
    
    private boolean isVariablesSelected() {
        boolean xValid = true;
        boolean yValid = true;
        String errorString = 
                Messages.getString("bpvisualizer_custom_define_var");
         if(selectedXVariable == null ||
               selectedXVariable.equals(SELECT_VAR)) {
             xValid = false;
           validationErrorBuffer.append(errorString);
           validationErrorBuffer.append(Messages.getString("bpvisualizer_custom_x_axis"));
           validationErrorBuffer.append("<br/>");
        }
         if(selectedYVariable == null ||
               selectedYVariable.equals(SELECT_VAR)) {
             yValid = false;
           validationErrorBuffer.append(errorString);
           validationErrorBuffer.append(Messages.getString("bpvisualizer_custom_y_axis"));
           validationErrorBuffer.append("<br/>");
           
         }
        
        return xValid && yValid;

    }
    
    private void validateNumericEntry(boolean testXVars) {
        String lVar = null;
        String uVar = null;
        String axis =  null;
        
        if(testXVars) {
            lVar = lowerLimit_x_axis;
            uVar = upperLimit_x_axis;
            axis = Messages.getString("bpvisualizer_custom_x_axis");
        } else {
            lVar = lowerLimit_y_axis;
            uVar = upperLimit_y_axis;
            axis = Messages.getString("bpvisualizer_custom_y_axis");
            
        }
        boolean lHasValue = lVar != null && lVar.length() != 0;
        if(lHasValue) {
            Double  dVal = DoubleValidator.getInstance().validate(lVar);
            Integer iVal = IntegerValidator.getInstance().validate(lVar);
            Float fVal = FloatValidator.getInstance().validate(lVar);
            Long lVal = LongValidator.getInstance().validate(lVar);
            if(dVal == null && iVal == null && fVal == null && 
                    lVal == null) {
                
               validationErrorBuffer.append(axis);
               validationErrorBuffer.append(Messages.getString("bpvisualizer_custom_lowerlimit_label"));
               validationErrorBuffer.append(Messages.getString("bpvisualizer_custom_no_numeric_var"));
               validationErrorBuffer.append("<br/>");
               hasValidEntries = false;
            }
        }
        boolean uHasValue = uVar != null && uVar.length() != 0;
        if(uHasValue) {
            Double  dVal = DoubleValidator.getInstance().validate(uVar);
            Integer iVal = IntegerValidator.getInstance().validate(uVar);
            Float fVal = FloatValidator.getInstance().validate(uVar);
            Long lVal = LongValidator.getInstance().validate(uVar);
            if(dVal == null && iVal == null && fVal == null && 
                    lVal == null) {
                
               validationErrorBuffer.append(axis);
               validationErrorBuffer.append(Messages.getString("bpvisualizer_custom_upperlimit_label"));
               validationErrorBuffer.append(Messages.getString("bpvisualizer_custom_no_numeric_var"));
               validationErrorBuffer.append("<br/>");
               hasValidEntries = false;
            }
        }
        
        if(lHasValue && uHasValue ) {
            // make sure upper limit value is high then lower limit value
            // convert all numeric types to double for the test
            if((DoubleValidator.getInstance().validate(lVar).doubleValue() -
               DoubleValidator.getInstance().validate(uVar).doubleValue()) > 0) {
               
               validationErrorBuffer.append(axis);
               validationErrorBuffer.append(Messages.getString("bpvisualizer_custom_u_is_smaller_l"));
               validationErrorBuffer.append("<br/>");
               hasValidEntries = false;
                
            }   
        }
        
    }
  
      
    private void validateDateEntry(boolean testXVars) {
        Date lVar = null;
        Date uVar = null;
        String axis =  null;
        
        if(testXVars) {
            lVar = dateLowerLimit_x_axis;
            uVar = dateUpperLimit_x_axis;
            axis = Messages.getString("bpvisualizer_custom_x_axis");
        } else {
            lVar = dateLowerLimit_y_axis;
            uVar = dateUpperLimit_y_axis;
            axis = Messages.getString("bpvisualizer_custom_y_axis");
            
        }
        boolean lHasValue = lVar != null;
        if(lHasValue) {
          if(lVar == null) {
                
               validationErrorBuffer.append(axis);
               validationErrorBuffer.append(Messages.getString("bpvisualizer_custom_lowerlimit_label"));
               validationErrorBuffer.append(Messages.getString("bpvisualizer_custom_no_date_var"));
               validationErrorBuffer.append("<br/>");
               hasValidEntries = false;
            }
        }
        boolean uHasValue =uVar != null ;
        if(uHasValue) {
           if(uVar == null) {
              
               validationErrorBuffer.append(axis);
               validationErrorBuffer.append(Messages.getString("bpvisualizer_custom_upperlimit_label"));
               validationErrorBuffer.append(Messages.getString("bpvisualizer_custom_no_date_var"));
               validationErrorBuffer.append("<br/>");
               hasValidEntries = false;
            }
        }
          if(lHasValue && uHasValue) {
            // make sure upper limit value is high then lower limit value
            // convert all numeric types to double for the test
            int compareValue =DateValidator.getInstance().compareDates(uVar,
                    lVar,null);
            if(compareValue == -1) {
               
               validationErrorBuffer.append(axis);
               validationErrorBuffer.append(Messages.getString("bpvisualizer_custom_u_is_smaller_l"));
               validationErrorBuffer.append("<br/>");
               hasValidEntries = false;
                
            }   
        }
   
    }
    
    
    private class BPELProcessWrapper {
        private String bpelName; 
        private RBPELProcess bpelProcess;
        
        
        private BPELProcessWrapper(String bpelURL, RBPELProcess bpelProcess) {
            setBpelName(bpelURL);
            setBpelProcess(bpelProcess);
        }
        
        private String getBpelName() {
            return bpelName;
        }
        private void setBpelName(String bpelURL) {
            int index = bpelURL.lastIndexOf("/");
            int bpelIndex =  bpelURL.lastIndexOf(".bpel");
            bpelName= bpelURL.substring(index+1,bpelIndex);
        }
        
        private RBPELProcess getBpelProcess() {
            return bpelProcess;
        }
        private void setBpelProcess(RBPELProcess bpelProcess) {
            this.bpelProcess = bpelProcess;
        }
     }
  
    
}
