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
 * @(#)StateController.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.cam.plugins.bpelse;

import com.sun.jbi.cam.plugins.bpelse.datamodel.State;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import javax.annotation.Resource;
import javax.faces.application.FacesMessage;
import javax.faces.context.FacesContext;
import javax.faces.model.DataModel;
import javax.faces.model.ListDataModel;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.transaction.UserTransaction;

import com.sun.data.provider.TableDataProvider;
import com.sun.data.provider.impl.ObjectListDataProvider;
import com.sun.data.provider.RowKey;
import com.sun.data.provider.FieldKey;
import com.sun.jbi.cam.common.GenericConstants;
import com.sun.jbi.cam.plugins.bpelse.table.util.Filter;
import com.sun.jbi.cam.plugins.bpelse.table.util.Group;
import com.sun.jbi.cam.plugins.bpelse.table.util.Select;
import com.sun.webui.jsf.component.TableRowGroup;
import javax.faces.context.ExternalContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
/**
 *
 * @author nnahata
 */
@SuppressWarnings("unchecked")
public class StateController {
    
    private State state;

    private DataModel model;
    
    @Resource
    private UserTransaction utx;

    //@PersistenceUnit(unitName = "cambpelsePU")
    private EntityManagerFactory emf;

    /*
    private Dynamic dynamic = null; // Dynamic util.
    private Table table = null; // Table component.
    private List<String> listColNames=null;
    private List<String> listColValExprs=null;
*/
    
    private TableRowGroup rowGroup = null;
    private TableDataProvider provider = null; // Data provider.
    //private Filter filter = null;
    
    public List<State> states = null;
    
    private int batchSize = 20;

    private int firstItem = 0;
    
    private Group groupA = null;
    private Filter groupAFilter = null;
    
    private DBManager dbManager = null;
    
    /** Creates a new instance of StateController */
    public StateController() {
        groupA = new Group(this);
        groupAFilter = new Filter(groupA);
        //dynamic = new Dynamic();
        
        dbManager = new DBManager();
    }
   
    
    public Group getGroupA(){
        if (groupA != null ){
            return groupA;
        }
        
        groupA = new Group(this);
        groupAFilter = new Filter(groupA);
        return groupA;
        
    }
   
    public Filter getGroupAFilter(){
        return groupAFilter;
    }       
    
    private EntityManager getEntityManager() {
        return emf.createEntityManager();
    }

    public State getState() {
        return state;
    }

    public void setState(State state) {
        this.state = state;
    }

    public DataModel getDetailStates() {
        return model;
    }

    public void setDetailStates(Collection<State> m) {
        model = new ListDataModel(new ArrayList(m));
    }

    public String createSetup() {
        this.state = new State();
        return "state_create";
    }

    public String create() {
        EntityManager em = getEntityManager();
        try {
            utx.begin();
            em.persist(state);
            utx.commit();
            addSuccessMessage("State was successfully created.");
        } catch (Exception ex) {
            try {
                addErrorMessage(ex.getLocalizedMessage());
                utx.rollback();
            } catch (Exception e) {
                addErrorMessage(e.getLocalizedMessage());
            }
        } finally {
            em.close();
        }
        return "state_list";
    }

    public String detailSetup() {
        setStateFromRequestParam();
        return "state_detail";
    }

    public String editSetup() {
        setStateFromRequestParam();
        return "state_edit";
    }

    public String edit() {
        EntityManager em = getEntityManager();
        try {
            utx.begin();
            state = em.merge(state);
            utx.commit();
            addSuccessMessage("State was successfully updated.");
        } catch (Exception ex) {
            try {
                addErrorMessage(ex.getLocalizedMessage());
                utx.rollback();
            } catch (Exception e) {
                addErrorMessage(e.getLocalizedMessage());
            }
        } finally {
            em.close();
        }
        return "state_list";
    }

    public String destroy() {
        EntityManager em = getEntityManager();
        try {
            utx.begin();
            State state = getStateFromRequestParam();
            state = em.merge(state);
            em.remove(state);
            utx.commit();
            addSuccessMessage("State was successfully deleted.");
        } catch (Exception ex) {
            try {
                addErrorMessage(ex.getLocalizedMessage());
                utx.rollback();
            } catch (Exception e) {
                addErrorMessage(e.getLocalizedMessage());
            }
        } finally {
            em.close();
        }
        return "state_list";
    }

    public State getStateFromRequestParam() {
        EntityManager em = getEntityManager();
        try{
            State o = (State) model.getRowData();
            o = em.merge(o);
            return o;
        } finally {
            em.close();
        }
    }

    public void setStateFromRequestParam() {
        State state = getStateFromRequestParam();
        setState(state);
    }

/*
    public TableDataProvider getStates() {
     System.out.println(">>>>Using ENTITY MANAGER for getting states");
//        if (provider != null ){
//            return provider;
//        }
        
        EntityManager em = getEntityManager();
        try{
            Query q = em.createQuery("select object(o) from State as o");
            q.setMaxResults(batchSize);
            q.setFirstResult(firstItem);
            //model = new ListDataModel(q.getResultList());
            //return model;
            
             provider = new ObjectListDataProvider(q.getResultList());
//            return q.getResultList();
        } catch(Throwable exception) {
            // Catch the org.apache.derby.client.am.SqlException Runtime 
            // exception thrown by the data layer.
            // This prevents a bad 500 error message in the Web App Servlet, i.e.,
            // javax.servlet.ServletException: Schema 'BPELSE_SCHEMA' does not exist
            // if the BPEL Schema is not created and ensures graceful behavior 
            // of the Web Application.
            exception.printStackTrace();
        } finally {
            em.close();
        }
        return provider;
    }
*/
   
    public TableDataProvider getStates(){
        System.out.println(">>>> Using DBManager for getting states");
        String suName = null;
        // update service unit name
        FacesContext context = FacesContext.getCurrentInstance();
        ExternalContext ex = context.getExternalContext();
        HttpServletRequest request = (HttpServletRequest)ex.getRequest();
        HttpSession session = request.getSession();  
        String viewType = 
                (String)session.getAttribute(GenericConstants.COMPONENT_TYPE);
        if(viewType.equals(GenericConstants.SU_TYPE)) {
            String name = 
              (String)session.getAttribute(GenericConstants.COMPONENT_NAME);
            String parentName = 
              (String)session.getAttribute(GenericConstants.COMPONENT_PNAME);
            suName = parentName + "-" + name;
        }
        provider = new ObjectListDataProvider(dbManager.getBPInstances(suName));
        return provider;
    }
   
    public TableDataProvider getNumericVars(){
    	TableDataProvider varprovider = new ObjectListDataProvider(dbManager.getNumericVars("BPEL_ID_0"));
        return varprovider;
    }
   
    /*
    public Table getTable() {
        if (table == null) {
            rowGroup = dynamic.getTableRowGroup("BPInstancesRowGroup",
                "#{state.states}", null);
            
            dynamic.setValueBinding(rowGroup,"binding","#{state.tableRowGroup}");
            
            listColNames = new ArrayList<String>();
            listColValExprs = new ArrayList<String>();

            listColNames.add("ID");
            listColNames.add("ENGINE");
            listColNames.add("STATUS");
            listColNames.add("BPEL");

            listColValExprs.add("#{bpinstance.value.id}");
            listColValExprs.add("#{bpinstance.value.engineid}");
            listColValExprs.add("#{bpinstance.value.status}");
            listColValExprs.add("#{bpinstance.value.bpelid}");
            
            dynamic.setTableRowGroupChildren(rowGroup, 
                    null,
                    null,
                    listColNames, listColValExprs);
            
            // Get table.
            table = dynamic.getTable("table1", "BPEL Instances");
            table.getChildren().add(rowGroup);
        }
        return table;
    }

    public void setTable(Table table){
        this.table = table;
    }
        
    public TableRowGroup getTableRowGroup(){
        return rowGroup;
    }
    */
    public void setTableRowGroup(TableRowGroup rowGroup){
        this.rowGroup = rowGroup;
    }
    
    public Filter getgroupAFilter(){
        return groupAFilter;
    }
    
    public static void addErrorMessage(String msg) {
        FacesMessage facesMsg = new FacesMessage(FacesMessage.SEVERITY_ERROR, msg, msg);
        FacesContext fc = FacesContext.getCurrentInstance();
        fc.addMessage(null, facesMsg);
    }

    public static void addSuccessMessage(String msg) {
        FacesMessage facesMsg = new FacesMessage(FacesMessage.SEVERITY_INFO, msg, msg);
        FacesContext fc = FacesContext.getCurrentInstance();
        fc.addMessage("successInfo", facesMsg);
    }

    public State findState(String id) {
        EntityManager em = getEntityManager();
        try{
            State o = (State) em.find(State.class, id);
            return o;
        } finally {
            em.close();
        }
    }

    public int getItemCount() {
        EntityManager em = getEntityManager();
        try{
            int count = ((Long) em.createQuery("select count(o) from State as o").getSingleResult()).intValue();
            return count;
        } finally {
            em.close();
        }
    }

    public int getFirstItem() {
        return firstItem;
    }

    public int getLastItem() {
        int size = getItemCount();
        return firstItem + batchSize > size ? size : firstItem + batchSize;
    }

    public int getBatchSize() {
        return batchSize;
    }

    public String next() {
        if (firstItem + batchSize < getItemCount()) {
            firstItem += batchSize;
        }
        return "state_list";
    }

    public String prev() {
        firstItem -= batchSize;
        if (firstItem < 0) {
            firstItem = 0;
        }
        return "state_list";
    }
    
    
    public String deleteAll() {
       // group object not valid therefore we don't have access to the
        // select object
        if(groupA ==  null) {
           return GenericConstants.SUCCESS;
        }
        //  get the ids of all done instances
        List<String> idList = dbManager.getInstancesIDDoneList();
        // create the filter (IN clause) of the query
        String deleteFilter = getFilterString(idList);
        // create the final list of queries to be executed as a single tx
        List<String> deleteQueryList = getDeleteQueryList(deleteFilter);

        dbManager.deleteSelectedBpelInstances(deleteQueryList);

        
        Select select = groupA.getSelect();
        // at the end of successful delete clear the Table Select PhaseListener 
        select.clear();
        groupA.clearProvider();
        return GenericConstants.SUCCESS;
       
    }
    
    public String deleteSelected() {
        // group object not valid therefore we don't have access to the
        // select object
        if(groupA ==  null) {
            return GenericConstants.SUCCESS;
        }
        
        Select select = groupA.getSelect();
        List<String> idList = getSelectedInstancesList(select);
        // create the filter (IN clause) of the query
        String deleteFilter = getFilterString(idList);
        // create the final list of queries to be executed as a single tx
        List<String> deleteQueryList = getDeleteQueryList(deleteFilter);

        dbManager.deleteSelectedBpelInstances(deleteQueryList);
        // at the end of successful delete clear the Table Select PhaseListener 
        select.clear();
        groupA.clearProvider();
        return GenericConstants.SUCCESS;
    }
    
    public boolean getDeleteSelectedDisabled() {
        return true;
    }
    
       private String getFilterString(List<String> instanceIdList) {
         StringBuffer filter  = new StringBuffer(" where stateid in (");
         for (String id : instanceIdList) {
             if(instanceIdList.lastIndexOf(id) == instanceIdList.size()-1) {
               filter.append("'" +id +"')");
             } else {
               filter.append("'" +id +"',");
             }
         }
         return filter.toString();
    }
    
    private List<String> getDeleteQueryList(String filter) {
        List<String> deleteQueryList =  new ArrayList() ;
        String deleteTemplate = "DELETE from BPELSE_SCHEMA.";
        deleteQueryList.add(deleteTemplate + "Crmp" + filter);
        deleteQueryList.add(deleteTemplate + "FaultHandlerData" + filter);
        deleteQueryList.add(deleteTemplate + "LastCheckPoint" + filter);
        deleteQueryList.add(deleteTemplate + "InstanceCorrelation" + filter);
        deleteQueryList.add(deleteTemplate + "ForEach" + filter);
        deleteQueryList.add(deleteTemplate + "WaitingIMA" + filter);
        deleteQueryList.add(deleteTemplate + "ScopeSnapshot" + filter);
        deleteQueryList.add(deleteTemplate + "Variable" + filter);
        // fix the filter for the state table
        deleteQueryList.add(deleteTemplate + "State" + filter);
        return deleteQueryList;
    }
    
    private List<String> getSelectedInstancesList(Select select) {
        List<String> selectedIntancesList = new ArrayList<String>();
        TableDataProvider instancesDP = groupA.getInstances();
        FieldKey idFieldKey = select.getFieldKeyForName(instancesDP,"id");
        RowKey[] selectedRowKeys = groupA.getTableRowGroup().getSelectedRowKeys();
        for(int i=0; i < selectedRowKeys.length; i++){
            String rowId = selectedRowKeys[i].getRowId();
            RowKey rowKey = instancesDP.getRowKey(rowId);
            boolean isSelected = select.getSelectedState(rowKey);
            if(isSelected) {
              String id = (String) provider.getValue(idFieldKey,rowKey);
              selectedIntancesList.add(id);
            }
        }
        
        return selectedIntancesList;
    }


}
