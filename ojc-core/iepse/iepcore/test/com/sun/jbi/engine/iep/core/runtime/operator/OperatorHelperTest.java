/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.engine.iep.core.runtime.operator;

import com.sun.jbi.engine.iep.core.runtime.operator.impl.SaveStream;
import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.Properties;
import junit.framework.TestCase;

/**
 *
 * @author radval
 */
public class OperatorHelperTest extends TestCase {
    
    public OperatorHelperTest(String testName) {
        super(testName);
    }            

    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Override
    protected void tearDown() throws Exception {
        super.tearDown();
    }

    public void testSaveStreamTemplate() throws Exception {
        OperatorHelper helper = OperatorHelper.getDefault();
        assertNotNull(helper);

        //load iep plan where we will add a save stream
        InputStream in  = OperatorHelperTest.class.getResourceAsStream("resources/dynamicallyAddSaveStream.iep");
        
        ByteArrayOutputStream writer = new ByteArrayOutputStream();
        BufferedInputStream ins = new BufferedInputStream(in);
        byte data[] = new byte[1024];
        int read = 0;
        while((read = ins.read(data)) != -1) {
            writer.write(data, 0, read);
        }
        
        String planContent = new String(writer.toByteArray(), "UTF-8");
        System.out.println("plan content " + planContent );
        Properties prop = new Properties();
        Properties configProp = new Properties();

        prop.put(OperatorConstants.PROP_CONFIG_PROPERTIES, configProp);

        prop.put(OperatorConstants.PROP_PLAN_ID, "Q1");
        prop.put(OperatorConstants.PROP_INSTANCE_ID, "Q1");
        prop.put(OperatorConstants.PROP_PLAN_NAME, "Q1");
        prop.put(OperatorConstants.PROP_PLAN_CONTENT, planContent);
        QueryPlan plan = new QueryPlanImpl(prop);

        assertNotNull(plan);

        //we expect two operators
        assertEquals(plan.getOperatorList().size(), 2);

        String jndiName = "jdbc/saveStream";
        String tableName = "STOCKTRANSACTIONS";

        SaveStream saveStream = helper.createAndAddSaveStream("StreamInput0", plan, jndiName, tableName, "true");
        assertNotNull(saveStream);
        
        //assert  isPreserveTable is set to true.
        assertEquals(true, saveStream.isPreserveTable());
        
        //check if plan now has our save stream
        Operator sop = plan.getOperatorByName(saveStream.getName());
        assertNotNull(sop);

        assertEquals(sop, saveStream);

        //check if save stream has StreamInput0 as its input
        String streamInputId = saveStream.getInputOperatorList().get(0).getId();
        assertNotNull(streamInputId);

        //check if stream input exist
        Operator si = plan.getOperatorById(streamInputId);
        assertNotNull(si);

        //check for save stream schema it should be same as stream input outut schema
        Schema s1 = saveStream.getOutputSchema();
        assertNotNull(s1);

        Schema s2 = si.getOutputSchema();
        assertNotNull(s2);

        assertEquals(s1, s2);

        //test if save stream has our jndiname and table name
        assertEquals(tableName, saveStream.getTableName());
        assertEquals(jndiName, saveStream.getDatabaseJndiName());

        
        //create another save stream with isGlobal set to false
        SaveStream saveStream2 = helper.createAndAddSaveStream("StreamInput0", plan, jndiName, tableName, "false");
        assertNotNull(saveStream2);
        
        //assert isPreserveTable is set to true.
        assertEquals(false, saveStream2.isPreserveTable());
        
    }




}
