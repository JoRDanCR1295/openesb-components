package com.sun.jbi.httpsoapbc;

import java.io.ByteArrayOutputStream;
import java.nio.ByteBuffer;

import javax.wsdl.Import;
import javax.wsdl.Types;
import javax.wsdl.WSDLException;
import javax.wsdl.extensions.schema.SchemaImport;
import javax.wsdl.factory.WSDLFactory;
import javax.wsdl.xml.WSDLWriter;

import junit.framework.TestCase;

import org.apache.coyote.tomcat5.CoyoteRequest;

import com.ibm.wsdl.DefinitionImpl;
import com.ibm.wsdl.extensions.schema.SchemaConstants;
import com.ibm.wsdl.extensions.schema.SchemaImpl;
/**
 * @author Sujit Biswas
 *
 */
public class WsdlQueryHelperTest extends TestCase {

    private CoyoteRequest req = new DummyCoyoteRequest();

    
    @Override
    protected void setUp() throws Exception {
	super.setUp();
    }

   
    @Override
    protected void tearDown() throws Exception {
	super.tearDown();
    }

    public void testGetServiceDescriptor() throws Exception {
	DefinitionImpl def = new DefinitionImpl();
	
	def.addNamespace("xsd", "http://www.w3.org/2001/XMLSchema");

	addImports(def);	
	//addSchema(def);	
	

	ByteArrayOutputStream baos = getAsStream(def);

	System.out.println(new String(baos.toByteArray()));

	WsdlQueryHelper helper = new WsdlQueryHelper(req, 9080, def, true);
	ByteBuffer buffer = helper.getServiceDescriptorAsByteBuffer();

	System.out.println(new String(buffer.array()));

    }


    private void addSchema(DefinitionImpl def) {
	Types types= def.createTypes();	
	
	
	SchemaImpl schema = new SchemaImpl();
	schema.setElementType(SchemaConstants.Q_ELEM_XSD_2001);
	schema.setRequired(false);
	
	SchemaImport imp= schema.createImport();	
	imp.setSchemaLocationURI("http://sbiswas-tecra.stc.com:9080/TestCA-sun-http-binding/TestBP/test.xsd");
	imp.setNamespaceURI("uri1");
	
	schema.addImport(imp);
	types.addExtensibilityElement(schema);
	def.setTypes(types);
    }

    private void addImports(DefinitionImpl def) {
	Import imp = def.createImport();
	Import imp1 = def.createImport();
	Import imp2 = def.createImport();

	imp.setLocationURI("http://sbiswas-tecra.stc.com:9080/TestCA-sun-http-binding/TestBP/test.wsdl");
	imp1.setLocationURI("http://sbiswas-tecra.stc.com:9080/TestCA-sun-http-binding/TestBP/test1.wsdl");
	imp2.setLocationURI("http://sbiswas-tecra.stc.com:18181/TestCA-sun-http-binding/TestBP/test2.wsdl");

	def.addImport(imp);
	def.addImport(imp1);
	def.addImport(imp2);
    }

    private ByteArrayOutputStream getAsStream(DefinitionImpl def) throws WSDLException {
	WSDLFactory wsdlFactory = (WSDLFactory) WSDLFactory.newInstance();
	WSDLWriter writer = (WSDLWriter) wsdlFactory.newWSDLWriter();
	ByteArrayOutputStream baos = new ByteArrayOutputStream();
	writer.writeWSDL(def, baos);
	return baos;
    }

    public void testGetServiceDescriptorAsByteBuffer() {
	// TODO
    }

    private class DummyCoyoteRequest extends CoyoteRequest {

	@Override
	public String getServerName() {

	    return "localhost";
	}

	@Override
	public int getServerPort() {
	    return 8000;
	}

    }

}
