/*******************************************************************************
 *  Copyright (c) 2005, 2006, 2007 Imola Informatica.
 *  All rights reserved. This program and the accompanying materials
 *  are made available under the terms of the LGPL License v2.1
 *  which accompanies this distribution, and is available at
 *  http://www.gnu.org/licenses/lgpl.html
 *******************************************************************************/
package it.imolinfo.jbi4cics.test.jbi;

import it.imolinfo.jbi4cics.jbi.Jbi4cicsComponent;
import it.imolinfo.jbi4cics.jbi.Jbi4cicsWSDLDeployer;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.nio.channels.FileChannel;
import java.util.List;

import javax.jbi.management.DeploymentException;

import junit.framework.TestCase;

import org.apache.servicemix.common.ServiceUnit;

/**
 * Test for jbi4CorbaWSDLDeployer class.
 * @author marcopiraccini
 */
public class WsdlCicsDeployTest extends TestCase {				
	
	
	public static final String wsdlDir = "src/test/etc/wsdl";
	
	public static final String workingPath = "target/test-wsdl-ext/temp";
	
	public WsdlCicsDeployTest() {}

	public WsdlCicsDeployTest(String name) {
		super(name);
	}
	
	public void setUp() {
		File dir = new File(workingPath);
		dir.mkdir();
	}
	   
	public void testNoExensibilityElementTest() {	
        
        String wsdlOrigFileName = wsdlDir + File.separator + "TestDeployNoExt.wsdl";
        String wsdlDestFileName = workingPath + File.separator + "TestDeployNoExt.wsdl";
        File wsdlOrigFile = new File(wsdlOrigFileName);
        File wsdlDestFile = new File(wsdlDestFileName);
        try {
            copyFile(wsdlOrigFile, wsdlDestFile);
        } catch (Exception e1) {
            e1.printStackTrace();
            fail(e1.getMessage());
        }
        
		Jbi4cicsComponent cicsComponent = new Jbi4cicsComponent();		   
		Jbi4cicsWSDLDeployer wsdlDeployer = new Jbi4cicsWSDLDeployer(cicsComponent);
        
		try {
			wsdlDeployer.deploy("pippo", workingPath);
			fail("No extended WSDL: a DeploymentException should be thrown");
		} catch (DeploymentException e) {			
		}		   		  		 	
	}
    
    public void testExensibilityElementProviderTest() {        
        Jbi4cicsComponent cicsComponent = new Jbi4cicsComponent();          
        Jbi4cicsWSDLDeployer wsdlDeployer = new Jbi4cicsWSDLDeployer(cicsComponent); 
        ServiceUnit su = new ServiceUnit();
        su.setComponent(cicsComponent);
        su.setName("testServiceUnit");
        su.setRootPath(workingPath);   
        
        String wsdlOrigFileName = wsdlDir + File.separator + "TestDeployExt.wsdl";
        String wsdlDestFileName = workingPath + File.separator + "TestDeployExt.wsdl";
        File wsdlOrigFile = new File(wsdlOrigFileName);
        File wsdlDestFile = new File(wsdlDestFileName);
        try {
            copyFile(wsdlOrigFile, wsdlDestFile);
        } catch (Exception e1) {
            e1.printStackTrace();
            fail(e1.getMessage());
        }                        
        try {
            
            List endpoints 
                = wsdlDeployer.getEndpointFromWsdl(wsdlDestFile);
            assertEquals(endpoints.size(), 1);
            System.out.println(endpoints.get(0));
            
        } catch (DeploymentException e) {
            e.printStackTrace();
            fail(e.getMessage());
        }                           
    }       
	    
    /**
     * Remove all the created files
     */
    public void tearDown() {
        File dir = new File(workingPath);
        
        File[] files = dir.listFiles();
        for (int i = 0; i < files.length; i++) {
            files[i].delete();
        }
        dir.delete();
    }    
        


	public void copyFile(File in, File out) throws Exception {
	    FileChannel sourceChannel = new
	    FileInputStream(in).getChannel();
	    FileChannel destinationChannel = new
	    FileOutputStream(out).getChannel();
	    sourceChannel.transferTo(0, sourceChannel.size(), destinationChannel);
	    // or
	    //  destinationChannel.transferFrom
	    //       (sourceChannel, 0, sourceChannel.size());
	    sourceChannel.close();
	    destinationChannel.close();
	}
      

}
