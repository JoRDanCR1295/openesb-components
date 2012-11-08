/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.sun.jbi.engine.tester;

import com.sun.jbi.configuration.ConfigPersistence;
import com.sun.jbi.configuration.RuntimeConfigurationHelper;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import javax.jbi.JBIException;
import javax.jbi.component.ComponentContext;
import javax.jbi.component.ComponentLifeCycle;
import javax.management.ObjectName;

/**
 *
 * @author radval
 */
public class TesterSELifeCycle implements ComponentLifeCycle {

    private ComponentContext mContext;
    
    private TesterRuntimeConfiguration mConfigMBean;
            
    private ScheduledExecutorService fileTriggerService = null;
    
    private ScheduledExecutorService responseHandlerService = null;
    
    private TestContext mTestContext = new TestContext();
    
    private RuntimeConfigurationHelper mRuntimeConfigHelper;
            
    private List<TestFolder> mTests = new ArrayList<TestFolder>();
    
    private ResponseHandler mHandler;
    
    
    public ObjectName getExtensionMBeanName() {
        return null;
    }

    public void init(ComponentContext context) throws JBIException {
        this.mContext = context;
        
        try {
        String configSchema = "";
        String configSchemaFileLoc = context.getInstallRoot() + File.separator + "META-INF" + File.separator + "componentDescription.xsd";
        File configSchemaFile = new File(configSchemaFileLoc);
        if (configSchemaFile.exists()) {
            configSchema = IOUtil.getText(configSchemaFileLoc, "UTF-8");
        }

        String configData = "";
        String configDataFileLoc = context.getInstallRoot() + File.separator + "META-INF" + File.separator + "componentConfiguration.xml";
        File configDataFile = new File(configDataFileLoc);
        if (configDataFile.exists()) {
            configData = IOUtil.getText(configDataFileLoc, "UTF-8");
        }

//      See "Prepare Installation Context" section in JBI 1.0 pr spec 6.4.2.1.1
        mConfigMBean = new TesterRuntimeConfiguration(mContext.getWorkspaceRoot() +
                    File.separator + ConfigPersistence.PERSISTENT_CONFIG_FILE_NAME, configSchema, configData);
        
        } catch(Exception ex) {
        	ex.printStackTrace();
        }
       
    }

    public void shutDown() throws JBIException {
    	try {
    		mRuntimeConfigHelper.unregisterMBean();
    	} catch(Exception ex) {
    		ex.printStackTrace();
    	}
    }

    public void start() throws JBIException {
	 try {
		    responseHandlerService = Executors.newScheduledThreadPool(1);
		    mHandler = new ResponseHandler(this.mContext, this.mTestContext);
	        responseHandlerService.scheduleWithFixedDelay(mHandler, 0L, 1L, TimeUnit.NANOSECONDS);
	        
	        fileTriggerService = Executors.newScheduledThreadPool(10);
	        
	        readConfig();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    public void stop() throws JBIException {
    	try {
	    	responseHandlerService.shutdown();
	    	fileTriggerService.shutdown();
	    	mHandler.shutdown();
	    	mTests.clear();
    	} catch(Throwable ex) {
    		ex.printStackTrace();
    	}
    	
    }
    
    private void readConfig() {
        try {
                        
            mRuntimeConfigHelper = new RuntimeConfigurationHelper(RuntimeConfigurationHelper.COMPONENT_TYPE_ENGINE,
                mContext.getComponentName(), mContext.getMBeanServer());
            mRuntimeConfigHelper.registerMBean(mConfigMBean);
        
            if(mConfigMBean != null) {
                String configFileLocation = mConfigMBean.getConfigFileLocation();
                if(configFileLocation != null) {
                    Properties prop = Util.readConfiguration(configFileLocation);
                    if(prop != null) {
                        String rootTestDir = prop.getProperty(Constants.PROP_CONFIG_ROOT_TEST_DIR);
                        if(rootTestDir != null) {
                            File file = new File(rootTestDir);
                            File[] testDirs = file.listFiles(new DirFileFilter());
                            for(int i = 0; i < testDirs.length; i++) {
                                File testDir = testDirs[i];
                                TestFolder folder = new TestFolder(testDir, mContext, mTestContext);
                                mTests.add(folder);
                                TestTrigger trigger = new TestTrigger(folder, fileTriggerService);
                                trigger.startPolling();
                            }
                        }
                    }
                }
            }    
        } catch(Exception ex) {
            ex.printStackTrace();
        }
    }

    class DirFileFilter implements FileFilter {

        public boolean accept(File pathname) {
            return pathname.isDirectory();
        }
        
    }
    
}
