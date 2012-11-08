/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */
/*
 * $RCSfile: ApplicationVariables.java,v $
 * $Revision: 1.1 $
 * $Date: 2007/10/02 19:30:41 $
 *
 * Copyright 2006 Sun Microsystems, Inc. All Rights Reserved.  
 */
package com.sun.jbi.snmpbc.mbeans;

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import javax.management.InvalidAttributeValueException;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;
import javax.management.openmbean.CompositeType;
import javax.management.openmbean.OpenType;
import javax.management.openmbean.OpenDataException;
import javax.management.openmbean.SimpleType;
import javax.management.openmbean.TabularData;
import javax.management.openmbean.TabularDataSupport;
import javax.management.openmbean.TabularType;

import com.sun.jbi.internationalization.Messages;

/**
 * ApplicationVariables provides support for environment variables in a JBI
 * Component.  This support is usually exposed through a MBean.  The main
 * purpose of this class is to provide an ApplicationVariables type that is
 * easy to use.
 *
 * @version      $Revision: 1.1 $
 *
 */
public class ApplicationVariables {

    /*
     * Messages object to create i18n messages 
     */
    private static final Messages mMessages =
        Messages.getMessages(ApplicationVariables.class);

    /*
     * Logger object to log messages
     */
    private static final Logger mLogger =
        Messages.getLogger(ApplicationVariables.class);

    /*
     * Row type used to represent the rows in the environment variable list.
     */
    private static CompositeType ENV_VAR_ROW_TYPE;

    /*
     * TabularType object used to represent the type of TabularData we have
     */
    private static TabularType ENV_VAR_TABULAR_TYPE;

    static {
        try {
            String[] envVarRowAttrNames = { "name", "value", "type" };
            String[] envVarRowAttrDesc = { "Application variable name", "Application variable value", "Application variable type" };
            OpenType[] envVarRowAttrTypes = { SimpleType.STRING, SimpleType.STRING, SimpleType.STRING };
            String[] envVarRowIndex = { "name" };
            
            if (ENV_VAR_ROW_TYPE == null) {
                ENV_VAR_ROW_TYPE = new CompositeType("NameValuePair",
                                                     "Application variable name and value pair",
                                                     envVarRowAttrNames,
                                                     envVarRowAttrDesc,
                                                     envVarRowAttrTypes);
            }
            
            if (ENV_VAR_TABULAR_TYPE == null) {
                ENV_VAR_TABULAR_TYPE = new TabularType("ApplicationVariableList",
                                                       "List of environment name and value pairs",
                                                       ENV_VAR_ROW_TYPE,
                                                       envVarRowIndex);
            }
        } catch (Exception ex) {
            throw new RuntimeException(ex);
        }

    }

    /* 
     * Data structure to hold environment variables.  We specifically used
     * MBean OpenTypes to better map to usage of ApplicationVariables
     */
    private TabularData mTabularData;

    /**
     * Default Constructor
     *
     * @exception    OpenDataException if an error occurs creating an ApplicationVariable
     */
    public ApplicationVariables() throws OpenDataException {
        mTabularData = new TabularDataSupport(ENV_VAR_TABULAR_TYPE);
    }

    /**
     * 
     *
     * @param        
     * @return       
     * @exception    
     * @see          
     */
    public String getValue(String name) {
        CompositeData data = mTabularData.get(new Object[] {name});
        return (String)data.get("value");
    }

    /**
     * 
     *
     * @param        
     * @return       
     * @exception    
     * @see          
     */
    public String getType(String name) {
        CompositeData data = mTabularData.get(new Object[] {name});
        return (String)data.get("type");
    }

    /**
     * 
     *
     * @param        
     * @return       
     * @exception    
     * @see          
     */
    public Set<String> getNames() {
        Set<String> names = new HashSet<String>();
        for (Iterator it = mTabularData.keySet().iterator(); it.hasNext(); ) {
            List key = (List)it.next();
            names.add((String)key.get(0));
        }
        return names;
    }

    /**
     * 
     *
     * @param        
     * @return       
     * @exception    
     * @see          
     */
    public boolean hasVariable(String name) {
        return mTabularData.containsKey(new Object[] {name});
    }

    /**
     * 
     *
     * @param        
     * @return       
     * @exception    
     * @see          
     */
    public void setVariable(String name, String value, String type) throws OpenDataException {
        CompositeData data = mTabularData.remove(new Object[] {name});
        data = new CompositeDataSupport(ENV_VAR_ROW_TYPE,
                                        new String[] { "name", "value", "type"},
                                        new Object[] { name, value, type });
        mTabularData.put(data);
    }

    /**
     * 
     *
     * @param        
     * @return       
     * @exception    
     * @see          
     */
    public void setVariable(CompositeData data) throws OpenDataException {
        mTabularData.remove(mTabularData.calculateIndex(data));
        mTabularData.put(data);
    }

    /**
     * 
     *
     * @param        
     * @return       
     * @exception    
     * @see          
     */
    public void addVariables(CompositeData[] vars) {
        mTabularData.putAll(vars);
    }

    /**
     * 
     *
     * @param        
     * @return       
     * @exception    
     * @see          
     */
    public void removeVariable(String name) {
        mTabularData.remove(new Object[] {name});
    }

    /**
     * 
     *
     * @param        
     * @return       
     * @exception    
     * @see          
     */
    public TabularData toTabularData() {
        return mTabularData;
    }

    /**
     * 
     *
     * @param        
     * @return       
     * @exception    
     * @see          
     */
    public void fromTabularData(TabularData tabularData) throws InvalidAttributeValueException {
        
        // First verify the tabularData object is valid.  Perform the following checks:
        //   1. Verify the size of the rowType is 3
        //   2. Verify the row index is exactly one
        //   3. Verify the mapKey is "name"

        // Verify the size of the rowType
        int itemSize = tabularData.getTabularType().getRowType().keySet().size();
        if (itemSize != 3) {
            throw new InvalidAttributeValueException(mMessages.getString("RTC_Invalid_Item_Size", itemSize));
        } 
        
        // Verify the row index is one and the mapKey is "name"
        List rowIndex = tabularData.getTabularType().getIndexNames();
        int rowIndexSize = rowIndex.size();
        if (rowIndexSize != 1) {
            throw new InvalidAttributeValueException(mMessages.getString("RTC_Invalid_RowIndex_Size", rowIndexSize));
        } else {
            String mapKey = (String) rowIndex.get(0);
            if (!mapKey.equals("name")) { // mapKey == null || "".equals(mapKey)) {
                throw new InvalidAttributeValueException(mMessages.getString("RTC_Invalid_RowIndex_Key", mapKey));
            }
        }

        mTabularData = tabularData;
    }


    public void test() throws Exception{
        CompositeData rowData = new CompositeDataSupport(ENV_VAR_ROW_TYPE,
                                                         new String[] {"name", "value", "type"},
                                                         new Object[] {"Alex", "Fung", "String"});
        mTabularData.put(rowData);
        rowData = new CompositeDataSupport(ENV_VAR_ROW_TYPE,
                                           new String[] {"name", "value", "type"},
                                           new Object[] {"Sherry", "Weng", "String"});
        mTabularData.put(rowData);

        System.out.println(getValue("Alex"));
        System.out.println(getType("Alex"));
        System.out.println(hasVariable("Alex"));
        System.out.println(hasVariable("Foo"));
        Set blah = getNames();
        for (Iterator<String> it = blah.iterator(); it.hasNext();) {
            String key = it.next();
            System.out.println("Key: " + key);
        }
        setVariable("Alex", "software", "password");
        blah = getNames();
        for (Iterator it = blah.iterator(); it.hasNext();) {
            System.out.println("Key: " + it.next());
        }
        System.out.println(getValue("Alex"));
        System.out.println(getType("Alex"));

  }

    public static void main(String[] args) throws Exception {

        ApplicationVariables var = new ApplicationVariables();
        var.test();
    }
}