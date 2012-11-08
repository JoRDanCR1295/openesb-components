package test.jbi.integration.test.framework;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;

import javax.xml.namespace.QName;

import test.jbi.integration.test.framework.SAAssembler.Redelivery.OnFailure;
import test.jbi.integration.testse.installer.TestHelper;

public class SAAssembler {
	
	private static final String DESCRIPTION_END = "</description>\n";
	private static final String DESCRIPTION_BEGIN = "<description>";
	private static final String NAME_END = "</name>\n";
	private static final String NAME_BEGIN = "<name>";
	private static final String XML = "<?xml version='1.0' encoding='UTF-8' standalone='no'?>\n";
	private static final String BEGIN_JBI = "<jbi xmlns='http://java.sun.com/xml/ns/jbi'" +
										" xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' " +
										" version='1.0' xsi:schemaLocation='http://java.sun.com/xml/ns/jbi ./jbi.xsd'>\n";
 
	private static final String END_JBI ="</jbi>\n";
	
	private static final String BEGIN_SA = "\t<service-assembly>\n";
	private static final String END_SA = "\t</service-assembly>\n";
	
	private static final String BEGIN_ID = "\t\t<identification>\n";
	private static final String END_ID = "\t\t</identification>\n";
	
	private static final String SERVICE_UNIT_END = "\t\t</service-unit>\n";
	private static final String SERVICE_UNIT_BEGIN = "\t\t<service-unit>\n";
	
	private ArrayList<SUAssembler> mSus = new ArrayList<SUAssembler>();
	private String name;
	private String description;
	
	private ArrayList<Object[]> connections = new ArrayList<Object[]>();
	
	
	public SAAssembler(String name, String desc){
		this.name = name.replace(' ', '_');
		this.description = desc;
		
	}

	public void addSUAssembler(SUAssembler su){
		mSus.add(su);
	}
	
	public boolean removeSUAssembler(SUAssembler su){
		return mSus.remove(su);
	}
	
	public String assemble(String workingDir) throws IOException{
		String base = workingDir + File.separator + name;
		//Create base directory
		File baseDir = new File(base);
		if(baseDir.exists()){
			TestHelper.deleteDirectory(baseDir);
		}
		baseDir.mkdirs();
		
		String meta = base + File.separator + "META-INF";
		File metaDir = new File(meta);
		metaDir.mkdir();
		
		StringBuffer buf = new StringBuffer();
		buf.append(XML);
		buf.append(BEGIN_JBI);
		buf.append(BEGIN_SA);

		buf.append(BEGIN_ID);
		buf.append("\t\t\t");
		buf.append(NAME_BEGIN);
		buf.append(name);
		buf.append(NAME_END);
		buf.append("\t\t\t");
		buf.append(DESCRIPTION_BEGIN);
		buf.append(description);
		buf.append(DESCRIPTION_END);
		
		buf.append(END_ID);
		for(Iterator<SUAssembler>iter=mSus.iterator(); iter.hasNext();){
			SUAssembler su = iter.next();
			buf.append(SERVICE_UNIT_BEGIN);
			
			buf.append("\t");
			buf.append(BEGIN_ID);

			buf.append("\t\t\t\t");
			buf.append(NAME_BEGIN);
			buf.append(su.getName());
			buf.append(NAME_END);
			
			buf.append("\t\t\t\t");
			buf.append(DESCRIPTION_BEGIN);
			buf.append(su.getDescription());
			buf.append(DESCRIPTION_END);

			buf.append("\t");
			buf.append(END_ID);

			buf.append("\t\t\t\t");
			buf.append("<target>\n");

				buf.append("\t\t\t\t\t");
				buf.append("<artifacts-zip>");
				buf.append(su.assemble(workingDir, base));
				buf.append("</artifacts-zip>\n");

				buf.append("\t\t\t\t\t");
				buf.append("<component-name>");
				buf.append(su.getComponentName());
				buf.append("</component-name>\n");

			buf.append("\t\t\t\t");
			buf.append("</target>\n");
			
			buf.append(SERVICE_UNIT_END);
			
		}
		
		//add all the connections
		buf.append("\t\t<connections xmlns='http://www.sun.com/jbi/qos'>\n");
		for(Iterator<Object[]> iter=connections.iterator(); iter.hasNext(); ){
			Object[] objs = iter.next();
			buf.append("\t\t\t<connection>\n");
			
			buf.append("\t\t\t\t<consumer xmlns:ns1='" + ((QName)objs[0]).getNamespaceURI() + "' endpoint-name='" + objs[1] + "' service-name='ns1:" +  ((QName)objs[0]).getLocalPart() + "'/>\n");
			buf.append("\t\t\t\t<provider xmlns:ns1='" + ((QName)objs[2]).getNamespaceURI() + "' endpoint-name='" + objs[3] + "' service-name='ns1:" +  ((QName)objs[2]).getLocalPart() + "'/>\n");
			
			if(objs[4] != null){
				Redelivery redelivery = (Redelivery)objs[4];
				buf.append("\t\t\t\t<redelivery xmlns='http://www.sun.com/jbi/qos/redelivery' maxAttempts='" + redelivery.maxAttempts + "' waitTime='" + redelivery.waitTime + "'>\n");
				buf.append("\t\t\t\t<on-failure>\n");
				if(redelivery.action == OnFailure.redirect){
					buf.append("\t\t\t\t\t<redirect xmlns:ns='").append(
							redelivery.redirectServiceName.getNamespaceURI())
							.append("' endpoint-name='").append(
									redelivery.redirectEndpointName).append(
									"' operation='").append(
									redelivery.redirectOperationName).append(
									"' service-name='ns:").append(
									redelivery.redirectServiceName
											.getLocalPart()).append("' />\n");
				}else{
					buf.append("\t\t\t\t\t<").append(redelivery.action).append("/>\n");
				}
				buf.append("\t\t\t\t</on-failure>\n");
				buf.append("\t\t\t\t</redelivery>\n");
			}
			if(objs[5]!=null){
				buf.append("<throttling xmlns='http://www.sun.com/jbi/qos/throttling' maximumConcurrencyLimit='").append(((Throttling)objs[5]).maximumConcurrencyLimit).append("'/>");
			}
			buf.append("\t\t\t</connection>\n");
		}
		
		buf.append("\t\t</connections>\n");
		

		buf.append(END_SA);
		buf.append(END_JBI);
		
		FileOutputStream out = new FileOutputStream(meta + File.separator + "jbi.xml");
		out.write(buf.toString().getBytes());
		out.close();
		String destJar = base + File.separator + name + ".jar";
		TestHelper.jarAllFiles(base, destJar);
		return destJar;
	}
	
	public void addConnection(QName consumerServiceName, String consumerEndpointName, QName providerServiceName, String providerEndpointName){
		addConnection(consumerServiceName, consumerEndpointName, providerServiceName, providerEndpointName, null, null);
	}
	
	public void addConnection(QName consumerServiceName, String consumerEndpointName, QName providerServiceName, String providerEndpointName, Redelivery redelivery){
		addConnection(consumerServiceName, consumerEndpointName, providerServiceName, providerEndpointName, redelivery, null);
	}
	
	public void addConnection(QName consumerServiceName, String consumerEndpointName, QName providerServiceName, String providerEndpointName, Redelivery redelivery, Throttling throttling){
		connections.add(new Object[]{consumerServiceName, consumerEndpointName, providerServiceName, providerEndpointName, redelivery, throttling});
	}

	static public class Redelivery{
	    public static enum OnFailure { redirect, error, suspend, delete};
	    private int maxAttempts;
	    private long waitTime;
	    private OnFailure action;
	    private String redirectEndpointName;
	    private String redirectOperationName;
	    private QName redirectServiceName; 
	    
	    public Redelivery(int maxAttempts, long waitTime, OnFailure action){
	    	this.waitTime = waitTime;
	    	this.maxAttempts = maxAttempts;
	    	this.action = action;
	    }
	    
	    public void setRedirectEndPoint(String endPointName, String operationName, QName serviceName){
	    	this.redirectEndpointName = endPointName;
	    	this.redirectOperationName = operationName;
	    	this.redirectServiceName = serviceName;
	    }
	}
	
	static public class Throttling{
		private int maximumConcurrencyLimit;
		public Throttling(int maxCC){
			this.maximumConcurrencyLimit = maxCC;
		}
	}
	
}
