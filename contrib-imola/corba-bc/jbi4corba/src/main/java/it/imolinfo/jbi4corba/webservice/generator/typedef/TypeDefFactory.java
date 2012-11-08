package it.imolinfo.jbi4corba.webservice.generator.typedef;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.ClassGenerationException;
import it.imolinfo.jbi4corba.jbi.Messages;
import it.imolinfo.jbi4corba.webservice.generator.TypeUtils;
import it.imolinfo.jbi4corba.webservice.generator.Util;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * TypeDef FactoryClass
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public final class TypeDefFactory {
	
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
	
	@SuppressWarnings("unchecked")
	public static TypeDef getTypeDef(String classesDir, String className, String helperClassName) throws ClassGenerationException {
		if (LOG.isDebugEnabled()) {
			LOG.debug("creating TypeDef for class: [" + className + "]  in class directory [" + classesDir);
		}
		Class helperClass = Util.classLoad(classesDir, helperClassName);
		// Looking for the Helper extract method (with Any parameter).
		Method extractMethod = null;
		String id = null;
		try {
			extractMethod = helperClass.getMethod("extract", org.omg.CORBA.Any.class);
			id = helperClass.getMethod("id",null ).invoke(null,null).toString();
		} catch (IllegalAccessException ex) {
			String msg = MESSAGES.getString("CRB001103_Error_recovering_extract_method_from_helper", 
	    			new Object[] {ex.getMessage()});
	        LOG.error(msg,ex);
	        throw new ClassGenerationException(msg,ex);
		} catch (InvocationTargetException ex) {
			String msg = MESSAGES.getString("CRB001103_Error_recovering_extract_method_from_helper", 
	    			new Object[] {ex.getMessage()});
	        LOG.error(msg,ex);
	        throw new ClassGenerationException(msg,ex);
		} catch (SecurityException ex) {
			String msg = MESSAGES.getString("CRB001103_Error_recovering_extract_method_from_helper", 
	    			new Object[] {ex.getMessage()});
	        LOG.error(msg,ex);
	        throw new ClassGenerationException(msg,ex);
		} catch (NoSuchMethodException ex) {
			String msg = MESSAGES.getString("CRB001103_Error_recovering_extract_method_from_helper", 
	    			new Object[] {ex.getMessage()});
	        LOG.error(msg,ex);
	        throw new ClassGenerationException(msg,ex);
		}
		Class aliasedClass = extractMethod.getReturnType();
		
		boolean isString = false;
		boolean isObject = false;
		
		// If, any, changes it to java.lang.Object
		if (aliasedClass.getName().startsWith("org.omg.CORBA.Any")) {		
			aliasedClass = java.lang.Object.class;
			isObject = true;
		}
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("Found TypeDef class [" + aliasedClass.getName() + "] for class [" + className + "]");
		}
		String aliasedTypeName = TypeUtils.getTypeName(aliasedClass);
		 
		// See if it's a primitive type or an array. Note that  
		// All the CORBA basic types are mapped on java primitive types, 
		// with the exceptions of string (mapped on java.lang.String)
		// and fixed (mapped on java.math.BigDecimal).
		boolean isArray = aliasedClass.isArray();		
		boolean isPrimitive = false;
		if (isArray) {
			// isPrimitive = TypeUtils.isPrimitive(aliasedClass.getComponentType().getName());
			isPrimitive = aliasedClass.getComponentType().isPrimitive();
		} else {
			// isPrimitive = TypeUtils.isPrimitive(aliasedClass.getName());
			isPrimitive = aliasedClass.isPrimitive();
		}
							
		
		if (isArray) {
			isString = aliasedClass.getComponentType().equals(java.lang.String.class);
		} else {
			isString = aliasedClass.equals(java.lang.String.class);
		}		
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("aliasedTypeName:" + aliasedTypeName);
			LOG.debug("isPrimitive:" + isPrimitive);
			LOG.debug("isString:" + isString);
			LOG.debug("isObject:" + isObject);
			LOG.debug("isArray:" + isArray);
		}
		
		// Istantiates the correct type
		TypeDef typeDef = null;
		if (isPrimitive || isString || isObject) {
			// For both the simple types, java.lang.String, java.lan.Object and the array
			// of these types.
			typeDef = new SimpleTypeDef(className, helperClassName, aliasedClass, id);			
		} else {
			if (!isArray) {
				typeDef = new ComplexTypeDef(className, helperClassName, aliasedClass, id);
			} else {
				typeDef = new ComplexArrayTypeDef(className, helperClassName, aliasedClass, id);
			}
		}		
		return typeDef;
	}
}
