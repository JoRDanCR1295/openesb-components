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
 * Test for jbi4CorbaWSDLDeployer class.
 * @author marcopiraccini
 */
public class WsdlDeployTest extends TestCase {


    public static final String suBaseDir = "src/test/etc/sus";	

    public static final String serviceUnitName = "testServiceUnit";

    public static final String rootPath = "target/test-wsdl-ext";


    // Files to test the deploy file filter 
    public static final String wsdlTestDirDeploy = "src/test/etc/wsdl-deploy";
    
    private String repodir = null;

    public WsdlDeployTest() {}

    public WsdlDeployTest(String name) {
        super(name);
    }

    public void setUp() {
        File dir = new File(rootPath);        
        dir.mkdir();
        
		repodir = System.getProperty("localRepository");		

		assertNotNull(repodir);
		assertFalse("".equals(repodir));
    }

    public void testNoExensibilityElementTest() {


        try {
        Jbi4CorbaRuntime corbaComponent = new Jbi4CorbaRuntime();
			Jbi4CorbaSUManager suManager = new Jbi4CorbaSUManager(
					corbaComponent);
			Jbi4CorbaLifeCycle lifeCycle = new Jbi4CorbaLifeCycle(
					corbaComponent);

			// Prepares the deploy WSDL
			String suPath = prepareWorkingPath(suBaseDir, "echo-su-noextension");

			// Copy the needed jar for the classes compile.
			JarListHelper.copyJarToTargetDir(repodir, suPath);

			// Copy the needed jar for the classes compile.
			JarListHelper.copyJarToTargetDir(repodir, suPath);

	        MockComponentContext context = new MockComponentContext();
	        context.setInstallRoot(suPath);
	        RuntimeContext.getInstance().setComponentContext(context);

	        lifeCycle.init(context);
	        suManager.setLifeCycle(lifeCycle);
	        context.setInstallRoot(suPath);

			suManager.init("pippo", suPath);
			suManager.deploy("pippo", suPath);
			suManager.start("pippo");

			// No WSDL extensions, so no endpoint should be deployed
			assertEquals(suManager.getDeployedEndpoints().size(), 0);

        } catch (Exception e) {
        	e.printStackTrace();
            fail(e.getMessage());
        }

    }

    public void testExensibilityElementProviderTest() {

        String suPath = null;
        try {
            // Prepares the deploy WSDL
            suPath = prepareWorkingPath(suBaseDir, "echo-su-provider");

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
            assertEquals(1, endpoints.size());
            System.out.println(endpoints.get(0));

        } catch (Exception e) {
            e.printStackTrace();
            fail(e.getMessage());
        } finally {
            deleteDir(new File(suPath));
        }
    }


    public void testProviderMultipleIDLStrangePaths() {

        //String suPath = null;
        //try {
        //    // Prepares the deploy WSDL
        //    suPath = prepareWorkingPath(suBaseDir, "echo-su-provider-multiple-idl-strange-paths");

        //    // Copy the needed jar for the classes compile.
        //    JarListHelper.copyJarToTargetDir(repodir, suPath);

        //    Jbi4CorbaRuntime corbaComponent = new Jbi4CorbaRuntime();
        //    Jbi4CorbaSUManager suManager = new Jbi4CorbaSUManager(corbaComponent);
        //    Jbi4CorbaLifeCycle lifeCycle = new Jbi4CorbaLifeCycle(corbaComponent);

        //    MockComponentContext context = new MockComponentContext();
        //    context.setInstallRoot(suPath);
        //    RuntimeContext.getInstance().setComponentContext(context);
        //    lifeCycle.init(context);
        //    suManager.setLifeCycle(lifeCycle);


        //    suManager.init(serviceUnitName, suPath);
        //    suManager.deploy(serviceUnitName, suPath);
            // if we start, try to connect to the orb
            // suManager.start(serviceUnitName);

        //    List endpoints = suManager.getDeployedEndpoints();
        //    assertEquals(1, endpoints.size());
        //    System.out.println(endpoints.get(0));

        //} catch (Exception e) {
        //    e.printStackTrace();
        //    fail(e.getMessage());
        // } finally {
        //    //deleteDir(new File(suPath));
        //}
    }

        public void testProviderMultipleIDLSameFiles() {

        //String suPath = null;
        //try {
        //    // Prepares the deploy WSDL
        //    suPath = prepareWorkingPath(suBaseDir, "echo-su-provider-multiple-idl-same-files");

            // Copy the needed jar for the classes compile.
        //    JarListHelper.copyJarToTargetDir(repodir, suPath);

        //    Jbi4CorbaRuntime corbaComponent = new Jbi4CorbaRuntime();
        //    Jbi4CorbaSUManager suManager = new Jbi4CorbaSUManager(corbaComponent);
        //    Jbi4CorbaLifeCycle lifeCycle = new Jbi4CorbaLifeCycle(corbaComponent);

        //    MockComponentContext context = new MockComponentContext();
        //    context.setInstallRoot(suPath);
        //    RuntimeContext.getInstance().setComponentContext(context);
        //    lifeCycle.init(context);
        //    suManager.setLifeCycle(lifeCycle);


        //    suManager.init(serviceUnitName, suPath);
        //    suManager.deploy(serviceUnitName, suPath);
        //    // if we start, try to connect to the orb
        //    // suManager.start(serviceUnitName);

        //    List endpoints = suManager.getDeployedEndpoints();
        //    assertEquals(2, endpoints.size());
        //    System.out.println(endpoints.get(0));

        //} catch (Exception e) {
        //    e.printStackTrace();
        //    fail(e.getMessage());
        //} finally {
            //deleteDir(new File(suPath));
        //}
    }

        public void testProviderMultipleIDLDifferentRoots() {

        //String suPath = null;
        //try {
            // Prepares the deploy WSDL
        //    suPath = prepareWorkingPath(suBaseDir, "echo-su-provider-multiple-idl-different-roots");

            // Copy the needed jar for the classes compile.
        //    JarListHelper.copyJarToTargetDir(repodir, suPath);

        //    Jbi4CorbaRuntime corbaComponent = new Jbi4CorbaRuntime();
        //    Jbi4CorbaSUManager suManager = new Jbi4CorbaSUManager(corbaComponent);
        //    Jbi4CorbaLifeCycle lifeCycle = new Jbi4CorbaLifeCycle(corbaComponent);

        //    MockComponentContext context = new MockComponentContext();
        //    context.setInstallRoot(suPath);
        //    RuntimeContext.getInstance().setComponentContext(context);
        //    lifeCycle.init(context);
        //    suManager.setLifeCycle(lifeCycle);


        //    suManager.init(serviceUnitName, suPath);
        //    suManager.deploy(serviceUnitName, suPath);
            // if we start, try to connect to the orb
            // suManager.start(serviceUnitName);

        //    List endpoints = suManager.getDeployedEndpoints();
        //    assertEquals(3, endpoints.size());
        //    System.out.println(endpoints.get(0));

        //} catch (Exception e) {
        //    e.printStackTrace();
        //    fail(e.getMessage());
        //} finally {
        //    //deleteDir(new File(suPath));
        //}
    }
    public void testProvider3gppMultipleInclude() {

        String suPath = null;
        try {
            // Prepares the deploy WSDL
            suPath = prepareWorkingPath(suBaseDir, "3gpp-su-provider-multiple-include");

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
            assertEquals(1, endpoints.size());
            System.out.println(endpoints.get(0));

        } catch (Exception e) {
            e.printStackTrace();
            fail(e.getMessage());
        } finally {
            //deleteDir(new File(suPath));
        }
    }

    public void testConsumer3gppMultipleInclude() {

        String suPath = null;
        try {
            // Prepares the deploy WSDL
            suPath = prepareWorkingPath(suBaseDir, "3gpp-su-consumer-multiple-include");

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
            assertEquals(1, endpoints.size());
            System.out.println(endpoints.get(0));

        } catch (Exception e) {
            e.printStackTrace();
            fail(e.getMessage());
        } finally {
            //deleteDir(new File(suPath));
        }
    }



    public void testExensibilityElementConsumerTest() {

        String suPath = null;
        try {
            // Prepares the deploy WSDL
            suPath = prepareWorkingPath(suBaseDir, "echo-su-consumer");

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
            assertEquals(1, endpoints.size());
            System.out.println(endpoints.get(0));

        } catch (Exception e) {
            e.printStackTrace();
            fail(e.getMessage());
        } finally {
            deleteDir(new File(suPath));
        }
    }



  /**
  * Tests if the endpoints are correctly recognized in a WSDL with both CORBA and
  * SOAP extensions 
  * The deployer should correctly load the WSDL, but no <code>JbiServiceDescriptor</code>
  * shuold be loaded.
  */
    public void testMultipleExtensibilityElements() {
        String suPath = null;
        try {
            // Prepares the deploy WSDL
            suPath = prepareWorkingPath(suBaseDir, "echo-su-multiple-extensions");

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
            assertEquals(1, endpoints.size());
            System.out.println(endpoints.get(0));

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
    
// COMMENTED! NOW THE jbi.xml is inspected...
//  /**
//  * Tests the wsdl file selection
//  */
//  public void testWrapperFileFilter() {
//
//      Jbi4CorbaComponent corbaComponent = new Jbi4CorbaComponent();
//      Jbi4CorbaWSDLDeployer wsdlDeployer = new Jbi4CorbaWSDLDeployer(corbaComponent);
//
//      File[] wsdls = wsdlDeployer.getDeployableWsdl(wsdlTestDirDeploy, wsdlDeployer.getDeployableFilesFilter());
//
//      System.out.println("Files retrieved: " + wsdls);
//      assertEquals(wsdls.length,1);
//
//      for (File wsdl: wsdls) {
//          System.out.println("File Name: " + wsdl.getName());
//      }
//
//      assertEquals(wsdls.length,1);
//      assertEquals(wsdls[0].getName(),"Echo.wsdl");
//
//  }


//  /**
//  * Tests if the
//  * The deployer should correctly load the WSDL, but no <code>JbiServiceDescriptor</code>
//  * shuold be loaded.
//  */
//  public void testImportWrapperWSDLFile() {

//  Jbi4CorbaComponent corbaComponent = new Jbi4CorbaComponent();
//  Jbi4CorbaWSDLDeployer wsdlDeployer = new Jbi4CorbaWSDLDeployer(corbaComponent);

//  String wsdlFileName = wsdlTestDirDeploy + File.separator + "EchoWrapper.wsdl";
//  File wsdlWrapperFile = new File(wsdlFileName);

//  ServiceUnit su = new ServiceUnit();
//  su.setComponent(corbaComponent);
//  su.setName(serviceUnitName);
//  su.setRootPath(wsdlTestDirDeploy);

//  List<JbiServiceDescriptor> sds = null;
//  // Test if the wsdl import work (no excpation shuold be thrown) and no endpoints shuold be loaded
//  try {
//  sds = wsdlDeployer.getServiceDescriptorsFromWsdl(su, wsdlWrapperFile);
//  } catch (DeploymentException e) {
//  e.printStackTrace();
//  fail(e.getMessage());
//  }
//  assertEquals(sds.size(),0);
//  }



}
