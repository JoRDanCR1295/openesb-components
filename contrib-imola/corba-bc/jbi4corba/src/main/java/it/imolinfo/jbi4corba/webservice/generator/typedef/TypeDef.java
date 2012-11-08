package it.imolinfo.jbi4corba.webservice.generator.typedef;

import it.imolinfo.jbi4corba.exception.ClassGenerationException;
import it.imolinfo.jbi4corba.webservice.generator.Util;

import java.io.File;

import org.objectweb.asm.Opcodes;

/**
 * TypeDef model.
 * This class models:
 * <ul>
 * <li/> simple typedefs
 * <li/> complex typedefs
 * <li/> simple array typedefs
 * <li/> complex array typedefs
 * </ul>
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a> 
 */
@SuppressWarnings("unchecked")
public abstract class TypeDef implements Opcodes {

	private static final String CLASS_EXTENSION = ".class";

	// Helper className
	private String helperClassName;
	
	// ClassName
	private String className;
	
	// Aliased Class 
	protected Class aliasedClass;
	
	// Aliased Class 
	protected String aliasedClassName;
	
	// TypeDef Class 
	protected Class typeDefClass;	
	
	protected String id;
	
	protected TypeDef(String className, String helperClassName, Class aliasedClass, String id) {
		this.className = className;
		this.helperClassName = helperClassName;		
		this.aliasedClass = aliasedClass;
		this.aliasedClassName = aliasedClass.getName();
		this.id = id;
	}
	
	public String getHelperClassName() {
		return helperClassName;
	}

	public String getClassName() {
		return className;
	}
	
	public Class getAliasedClass() {
		return aliasedClass;
	}			

	public String getAliasedClassName() {
		return aliasedClassName;
	}

	public String toString() {
		StringBuffer bf = new StringBuffer();
		bf.append(" [").append(className)
			.append("] with Helper [").append(helperClassName)
			.append("] is an alias of [").append(aliasedClass).append("]");
		return bf.toString();
	}
	
		
	/**
	 * Return the relative class name.
	 * @return
	 */
	protected String getClassRelativeFileName() {
		return this.className.replace('.', File.separatorChar).concat(CLASS_EXTENSION);		
	}
	
	/**
	 * Return the relative class name.
	 * @return
	 * @throws ClassGenerationException 
	 */
	public Class getHelperClass(String classesDir) throws ClassGenerationException {
		return Util.classLoad(classesDir, helperClassName);			
	}	
	
	public Class getTypeDefClass() {
		return typeDefClass;
	}

	public void setTypeDefClass(Class typeDefClass) {
		this.typeDefClass = typeDefClass;
	}
			
	public String getId() {
		return id;
	}

	// Returns the typeDf object from an instance of the wrapped object.
	// For example: java.lang.String -> mystring
	public abstract Object getTypeDefObject(ClassLoader classLoader, Object obj) throws ClassNotFoundException, InstantiationException, IllegalAccessException, SecurityException, NoSuchFieldException;
	
	// Returns the aliased object. For example: mystring -> java.lang.String 
	public abstract Object getAliasedObject(Object obj) throws SecurityException, NoSuchFieldException, IllegalArgumentException, IllegalAccessException;

	// Creates the typedef class
	public abstract void createTypeDefClass(String classesDir) throws ClassGenerationException;	
		
}
