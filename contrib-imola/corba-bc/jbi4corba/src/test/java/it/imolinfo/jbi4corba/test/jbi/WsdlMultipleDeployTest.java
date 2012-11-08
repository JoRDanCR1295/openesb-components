 /****************************************************************************
 * Copyright (c) 2005, 2006, 2007, 2008, 2009 Imola Informatica.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the LGPL License v2.1
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 ****************************************************************************/

package it.imolinfo.jbi4corba.test.jbi;

import it.imolinfo.jbi4corba.jbi.component.Jbi4CorbaLifeCycle;
import it.imolinfo.jbi4corba.jbi.component.Jbi4CorbaRuntime;
import it.imolinfo.jbi4corba.jbi.component.Jbi4CorbaSUManager;
import it.imolinfo.jbi4corba.jbi.component.runtime.RuntimeContext;
import it.imolinfo.jbi4corba.test.config.JarListHelper;
import it.imolinfo.jbi4corba.test.mock.MockComponentContext;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import junit.framework.TestCase;

/**
 *
 * @author luca
 */
public class WsdlMultipleDeployTest extends TestCase{
    
    public static final String suBaseDir = "src/test/etc/sus";	

    public static final String serviceUnitName = "testServiceUnit";

    public static final String rootPath = "target/test-wsdl-ext";
    
    private String repodir = null;


    // Files to test the deploy file filter 
    public static final String wsdlTestDirDeploy = "src/test/etc/wsdl-deploy";

    public WsdlMultipleDeployTest() {}

    public WsdlMultipleDeployTest(String name) {
        super(name);
    }

    public void setUp() {
        File dir = new File(rootPath);        
        dir.mkdir();
        
		repodir = System.getProperty("localRepository");		

		assertNotNull(repodir, "The localRepositry variable must be set");
		assertFalse("".equals(repodir));
    }

    
    public void testMultipleInterface() {
        
        String suPath = null;
        try {
            // Prepares the deploy WSDL
            suPath = prepareWorkingPath(suBaseDir, "echo-su-provider-multiple-int");  
            
            // MARCO: added to correcly load the neede jar.
            // Copy the needed jar for the classes compile.
            JarListHelper.copyJarToTargetDir(repodir, suPath);    
        
            Jbi4CorbaRuntime corbaComponent = new Jbi4CorbaRuntime();          
            Jbi4CorbaSUManager suManager = new Jbi4CorbaSUManager(corbaComponent);
            Jbi4CorbaLifeCycle lifeCycle = new Jbi4CorbaLifeCycle(corbaComponent);
            
            MockComponentContext context = new MockComponentContext();
            context.setInstallRoot(suPath);
            RuntimeContext.getInstance().setComponentContext(context);                 
            lifeCycle.init(context);
            suManager.setLifeCycle(lifeCycle);
            
            
            suManager.init(serviceUnitName, suPath);
            suManager.deploy(serviceUnitName, suPath);
            // if we start, try to connect to the orb
            // suManager.start(serviceUnitName);

            List endpoints = suManager.getDeployedEndpoints();
            assertEquals(2, endpoints.size());            
            System.out.println(endpoints.get(0));  
            System.out.println(endpoints.get(1));
            
        } catch (Exception e) {
            e.printStackTrace();
            fail(e.getMessage());                
        } finally {
            deleteDir(new File(suPath));
        }
    }	

    public void testConsumerMultipleInterface() {
      
      String suPath = null;
      try {
          // Prepares the deploy WSDL
          suPath = prepareWorkingPath(suBaseDir, "echo-su-consumer-multiple");  
          
          // MARCO: added to correcly load the neede jar.
          // Copy the needed jar for the classes compile.
          JarListHelper.copyJarToTargetDir(repodir, suPath);    
      
          Jbi4CorbaRuntime corbaComponent = new Jbi4CorbaRuntime();          
          Jbi4CorbaSUManager suManager = new Jbi4CorbaSUManager(corbaComponent);
          Jbi4CorbaLifeCycle lifeCycle = new Jbi4CorbaLifeCycle(corbaComponent);
          
          MockComponentContext context = new MockComponentContext();
          context.setInstallRoot(suPath);
          RuntimeContext.getInstance().setComponentContext(context);                 
          lifeCycle.init(context);
          suManager.setLifeCycle(lifeCycle);
          
          
          suManager.init(serviceUnitName, suPath);
          suManager.deploy(serviceUnitName, suPath);
          // if we start, try to connect to the orb
          // suManager.start(serviceUnitName);

          List endpoints = suManager.getDeployedEndpoints();
          assertEquals(2, endpoints.size());            
          System.out.println(endpoints.get(0));  
          System.out.println(endpoints.get(1));
          
      } catch (Exception e) {
          e.printStackTrace();
          fail(e.getMessage());                
      } finally {
          deleteDir(new File(suPath));
      }
  }	


  
    /**
     * Remove all the created files
     */
    @Override
    public void tearDown() {
       File dir = new File(rootPath);
        if (dir.exists()) {        
            File[] files = dir.listFiles();
            for (int i = 0; i < files.length; i++) {
                files[i].delete();
            }
            dir.delete();
        }
        
    }    

    /**
     * Copy the WSDL on the working path.
     * @param fileName
     * @return the SU directory
     */
    public String prepareWorkingPath(String wsdlSrcDir, String suName) {
        String wsdlOrig= wsdlSrcDir + File.separator + suName;
        String wsdlDest = rootPath + File.separator + suName;
        File wsdlOrigDir = new File(wsdlOrig);
        File wsdlDestDir = new File(wsdlDest);
        try {
            copyDirectory(wsdlOrigDir, wsdlDestDir);
        } catch (Exception e1) {
            e1.printStackTrace();
            fail(e1.getMessage());
        }  
        return wsdlDestDir.getAbsolutePath();
    }

    /**
     * Copy directory.
     * S
     * @param sourceLocation
     * @param targetLocation
     * 
     * @throws IOException
     */
    public static void copyDirectory(File sourceLocation , File targetLocation)
    throws IOException {

        if (sourceLocation.isDirectory()) {
            if (!targetLocation.exists()) {
                targetLocation.mkdir();
            }

            String[] children = sourceLocation.list();
            for (int i=0; i<children.length; i++) {
                copyDirectory(new File(sourceLocation, children[i]),
                              new File(targetLocation, children[i]));
            }
        } else {

            InputStream in = new FileInputStream(sourceLocation);
            OutputStream out = new FileOutputStream(targetLocation);

            // Copy the bits from instream to outstream
            byte[] buf = new byte[1024];
            int len;
            while ((len = in.read(buf)) > 0) {
                out.write(buf, 0, len);
            }
            in.close();
            out.close();
        }
    }   

    /**
     * recursively removes all the directory
     * @param dir
     * @return
     */
    public boolean deleteDir(File dir) {
        if (dir.isDirectory()) {
            String[] children = dir.list();
            for (int i=0; i<children.length; i++) {
                boolean success = deleteDir(new File(dir, children[i]));
                if (!success) {
                    return false;
                }
            }
        }    
        // The directory is now empty so we can delete it
        return dir.delete();
    }
    




}
