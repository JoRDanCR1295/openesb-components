package it.imolinfo.jbi4corba.webservice.generator.typedef;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.ClassGenerationException;
import it.imolinfo.jbi4corba.exception.Jbi4CorbaException;
import it.imolinfo.jbi4corba.jbi.Messages;
import it.imolinfo.jbi4corba.webservice.generator.TypeUtils;
import it.imolinfo.jbi4corba.webservice.generator.Util;

import java.io.File;
import java.io.IOException;
import java.lang.reflect.Array;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

/**
 * TypeDef Helper class
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 * 
 */

public final class TypeDefUtil {

	private static final String CLASS_EXTENSION = ".class";

	private static final String HELPER_CLASS = "Helper.class";
	
	/**
	 * Class logger
	 */
	private static final Logger LOG = LoggerFactory
			.getLogger(TypeDefUtil.class);
	
	/**
	 * Error and log messages
	 */
	private static final Messages MESSAGES = 
		  	Messages.getMessages(TypeDefUtil.class);

	/**
	 * Returns the list of the typedef helper classes (to be build). The typedef
	 * types are recognized because the helper is present but with no type
	 * class.
	 * 
	 * @param workdirsrc
	 * @return
	 * @throws Jbi4CorbaException
	 */
	public static Map<String, TypeDef> getTypeDefs(String classesdir)
			throws Jbi4CorbaException {

		Map<String, TypeDef> typeDefMap = new HashMap<String, TypeDef>();

		if (classesdir == null) {
			LOG
					.warn("CRB001101_getTypeDefTypes_cannot_work_workdirsrc_is_null");
			LOG.debug("<<<<< getTypeDefTypes - end");
			return typeDefMap;
		}

		try {
			// Gets all the sources
			List<File> files = Util.findFilesFromSourceDirectory(classesdir, CLASS_EXTENSION);

			for (File file : files) {
				String filePath = file.getAbsolutePath(); 
				if (filePath.endsWith(HELPER_CLASS)) {
					// Typedef candidate. Try if the sources contain the
					// class source name without the "helper". If not, it's a
					// Typedef.
					String typeName = filePath.substring(0,
						filePath.length() - HELPER_CLASS.length()).concat(CLASS_EXTENSION);
					
					LOG.debug("Looking for type:" + typeName);
					if (!(containsFile(typeName, files))) {
						// It's an helper
						LOG.debug("Type:" + typeName + " not found. The source: "
							+ filePath + "it's an helper of a typedef");
						
						String typeDefHelperClassName = Util.getClassName(file, classesdir, CLASS_EXTENSION);						
						String typeDefClassName = Util.getClassName(typeName, classesdir, CLASS_EXTENSION);
						
						// Creates the TypeDef Object						
						TypeDef typeDef 
							= TypeDefFactory.getTypeDef(classesdir, typeDefClassName, typeDefHelperClassName);
							
						if (typeDef != null) {						
							typeDefMap.put(typeDef.getClassName(), typeDef);
						}																			
					}
				}
			}		
		} catch (IOException ex) {		
	    	String msg = MESSAGES.getString("CRB001102_Error_creating_typedefs_classes", 
	    			new Object[] {ex.getMessage()});
	        LOG.error(msg,ex);
	        throw new ClassGenerationException(msg,ex);	        	
		}
		return typeDefMap;
	}
	
	/**
	 * True if the <code>files</code> contain the <code>absoluteFileName</code>.
	 * @param absoluteFileName The file name to look for
	 * @param files The file list
	 * @return
	 * @throws IOException
	 */
	private static boolean containsFile(String absoluteFileName, List<File> files) 
			throws IOException {
		boolean ret = false;
		for (File file: files) {
			if (file.getAbsolutePath().equals(absoluteFileName)) {
				ret = true;
			}
		}
		return ret;
	}
	
	
	/**
	 * Gets the TypeDef from the ID. 
	 * This should be done also saving the correspondance between id/typedef in a more effective way.
	 * @param typeDefs
	 * @param id
	 * @return
	 */
	public static TypeDef getTypeDefFromID(Map<String, TypeDef> typeDefs, String id) {
		Iterator<TypeDef> it =typeDefs.values().iterator();
		boolean found = false;
		TypeDef ret = null;
		while ((it.hasNext()) && (!found)) {
			TypeDef typeDef = (TypeDef)it.next();
			if (typeDef.getId() != null) {
				if (typeDef.getId().equals(id)) {
					found = true;
					ret = typeDef;
				}
			} else {
				LOG.info("TypeDef.id is null for TypeDef:" + typeDef.getClassName());
			}
		}
		return ret;
	}
	
	/**
	 * Fills a multidimentional array using recursion. This is necessary, since
	 * a String[][] cannot be assigned to a Object[][] in java.
	 * @param src
	 * @param dest
	 * @param typeClass
	 * @return
	 */
	public static Object fillArray(Object src, Object dest, Class typeClass) {				
				
		int srcDim = Array.getLength(src);
		
		if (dest == null) {
			dest = Array.newInstance(typeClass, srcDim);
		}
		
		for (int i = 0; i < srcDim; i++) {			
			if (LOG.isDebugEnabled()) {
				LOG.debug("Analyzing myarr: " + Array.get(src, i)); 					
				LOG.debug("typeClass: " + typeClass);
				LOG.debug("typeClass classloader: " + typeClass.getClassLoader());
			}
			if (Array.get(src, i) != null) {
				if (LOG.isDebugEnabled()) {
					LOG.debug("Analyzing myarr: " + Array.get(src, i).getClass().getName());
					LOG.debug("Analyzing myarr classloader: " + Array.get(src, i).getClass().getClassLoader());	
					LOG.debug("myarr[i].getClass().isArray(): " + Array.get(src, i).getClass().isArray());
				}
				if (Array.get(src, i).getClass().isArray()) {
					// Recursion....		
					Object myNewArr = fillArray(Array.get(src, i), Array.get(dest,i), typeClass.getComponentType());
					Array.set(dest, i, myNewArr);						
				} else {
					if (LOG.isDebugEnabled()) {
						LOG.debug("Setting on" + dest.getClass().getName() + " the value: " + Array.get(src, i));
					}
					Array.set(dest, i, Array.get(src, i));				
				}
			}
		}
		return dest;
	}	
	
}
