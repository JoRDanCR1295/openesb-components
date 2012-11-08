package it.imolinfo.jbi4corba.webservice.generator.typedef;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.ClassGenerationException;
import it.imolinfo.jbi4corba.webservice.generator.Util;
import it.imolinfo.jbi4corba.webservice.generator.bcm.ByteCodeManipulationUtil;

import java.io.File;
import java.lang.reflect.Field;

import javax.xml.bind.annotation.XmlTransient;
import javax.xml.bind.annotation.XmlType;
import javax.xml.bind.annotation.XmlValue;

import org.apache.commons.lang.ArrayUtils;
import org.apache.cxf.helpers.ServiceUtils;
import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

/**
 * TypeDef for Simple types (basic types + String) and array/sequences of simple types.
 * 
 * The class generated is:
 *
 * <pre>
 * {@code
 * @XmlType
 * public class myint {
 *    @XmlTransient
 *    final static public myintHelper helperClass = myintHelper.class;
 *    @XmlValue
 *    public int value;
 * }
 * }
 * </pre>
 * 
 * Or, for arrays and sequences:
 * 
 * <pre>
 * {@code
 * @XmlType
 * public class myint {
 *    //@XmlTransient
 *    // final static public myintHelper helperClass = myintHelper.class;
 *    @XmlValue
 *    public int[] value;
 * }
 * }
 * </pre>
 * 
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a> 
 */
@SuppressWarnings("unchecked")
public class SimpleTypeDef extends TypeDef  {
	
	/**
	 * Class logger
	 */
	private static final Logger LOG = LoggerFactory
			.getLogger(SimpleTypeDef.class);	
	
	protected SimpleTypeDef(String className, String helperClassName, Class aliasedClass, String id) {
		super(className, helperClassName, aliasedClass, id);		
	}
	
	/**
	 * Creates the class for the typedef.
	 * 
	 */
	public void createTypeDefClass(String classesDir) throws ClassGenerationException {
		
		String namespace = ServiceUtils.makeNamespaceFromClassName(this.getClassName(), "http");
		
		// I have to calculate this values (i cannot load the class...doesn't exist yet)
		String classInternalName = this.getClassName().replace('.','/');
		String classDescriptor = "L" + classInternalName + ";";
		String aliasedClassDescriptor =  Type.getDescriptor(aliasedClass);
		String objectArrayClassDescriptor = Type.getDescriptor(java.lang.Object[].class);
				
		// helper class
		Class helperClass = this.getHelperClass(classesDir);  		
		String helperDescriptor = Type.getDescriptor(helperClass);	
				
		// Class, Object and jaxb annotations
		String javaLangClassDescriptor = Type.getDescriptor(Class.class);
		String javaLangObjectInternalName = Type.getInternalName(java.lang.Object.class);
		String xmlTypeDescriptor = Type.getDescriptor(XmlType.class);
		String xmlValueDescriptor = Type.getDescriptor(XmlValue.class);
		String xmlTransientDescriptor = Type.getDescriptor(XmlTransient.class);
								
		ClassWriter cw = new ClassWriter(true);
		
		// The class
		cw.visit(Opcodes.V1_5, ACC_PUBLIC + ACC_SUPER, classInternalName ,null, javaLangObjectInternalName, null);
		
		// The class XmlType Annotation		
		AnnotationVisitor av = cw.visitAnnotation(xmlTypeDescriptor, true);		
		av.visit("namespace", namespace);
		av.visitEnd();	
		
		/* 
		 * @XmlTransient
		 * final static private Class helperClass = null; 		// cannot be inited (null) 	
		 */			
		/*
		FieldVisitor fvHelperClass =  cw.visitField(ACC_PRIVATE + ACC_STATIC + ACC_FINAL
				, "helperClass", javaLangClassDescriptor, null, null);
		AnnotationVisitor avHelper = fvHelperClass.visitAnnotation(xmlTransientDescriptor, true);
		avHelper.visitEnd();			
		fvHelperClass.visitEnd();		
*/												
		/* The field with the XmlType annotation.
		 * For example: 	
		 * @XmlValue
		 * public int value;
		 */
		FieldVisitor fv = null;
		// if (this.aliasedClass.isArray()) {
	    // Array (always Object[]).
		//	fv = cw.visitField(ACC_PUBLIC, "value", objectArrayClassDescriptor, null, null);			
		//} else {
		// Simple type
			fv = cw.visitField(ACC_PUBLIC, "value", aliasedClassDescriptor, null, null);
		// }		
			
		// This test is necesary to avoid a jaxb bug (with jaxb-impl-2.1.9).
		if (!aliasedClass.equals(java.lang.Object.class)) {
			AnnotationVisitor av2 = fv.visitAnnotation(xmlValueDescriptor, true);
			av2.visitEnd();		
			fv.visitEnd();
		}
					
						
		// Class init. Inits the static "helperClass" field.	
		/*
		MethodVisitor mv = cw.visitMethod(ACC_STATIC, "<clinit>", "()V", null, null);
		mv.visitCode();
		Label lclinit = new Label();
		mv.visitLabel(lclinit);
		mv.visitLineNumber(10, lclinit);
		mv.visitLdcInsn(Type.getType(helperDescriptor));		
		mv.visitFieldInsn(PUTSTATIC, classInternalName, "helperClass", javaLangClassDescriptor);
		Label lclinit1 = new Label();
		mv.visitLabel(lclinit1);
		mv.visitLineNumber(7, lclinit1);
		mv.visitInsn(RETURN);
		mv.visitMaxs(1, 0);
		mv.visitEnd();
		*/
		
		ByteCodeManipulationUtil bcmUtil = new ByteCodeManipulationUtil();
		// SETTER
		bcmUtil.createSetter(cw, classInternalName, "value", aliasedClassDescriptor);
		
		// GETTER
        // bcmUtil.createGetter(cw, classInternalName, "value", aliasedClassDescriptor, false);
		
		// Default constructor		 
		MethodVisitor mvinit = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
		mvinit.visitCode();
		Label linit = new Label();
		mvinit.visitLabel(linit);
		mvinit.visitLineNumber(7, linit);
		mvinit.visitVarInsn(ALOAD, 0);
		mvinit.visitMethodInsn(INVOKESPECIAL, javaLangObjectInternalName, "<init>", "()V");
		mvinit.visitInsn(RETURN);
		Label linit1 = new Label();
		mvinit.visitLabel(linit1);
		mvinit.visitLocalVariable("this", classDescriptor, null, linit, linit1, 0);
		mvinit.visitMaxs(1, 1);
		mvinit.visitEnd();
		
		cw.visitEnd();
		byte[] bytecode = cw.toByteArray();
		
		String relativeClassName = getClassRelativeFileName();
		
		if (LOG.isDebugEnabled()) {
			LOG.debug("Writing out TypeDef class:" + classesDir + File.separatorChar + relativeClassName);
		}
		// Writes out the class
		Util.saveAsJavaClass(classesDir, relativeClassName, bytecode);		
	}
	
	@Override
	public Object getTypeDefObject(ClassLoader classLoader, Object obj) throws ClassNotFoundException, InstantiationException, IllegalAccessException, SecurityException, NoSuchFieldException {
		Class classDefinition = classLoader.loadClass(getClassName());
        Object typeDefObject = classDefinition.newInstance();
        Field value = typeDefObject.getClass().getField("value");
        LOG.debug("Trying to set: " + obj + " of object: " + obj + " on object:" + typeDefObject);    	
    	// This seems the only way to cast a Object[] to a String[]!!!!!
        if (obj.getClass().isArray()) {
        	
        	if (LOG.isDebugEnabled()) {
        		LOG.debug("Input: " + ArrayUtils.toString(obj));
        	}
         	Class aliasedClassCorrectCL = Class.forName(this.getAliasedClassName(), false, classLoader);        	        	        	        
        	Object ob = TypeDefUtil.fillArray(obj, null, aliasedClassCorrectCL.getComponentType());
        	if (LOG.isDebugEnabled()) {
        		LOG.debug("Returned: " + ArrayUtils.toString(ob));
        	}
        	value.set(typeDefObject,ob);
        	
        	/*
        	Vector v = new Vector();
        	Object[] myarr = (Object[]) obj;  
        	for (int i = 0; i < myarr.length; i++) {      
        		v.add(myarr[i]);        		        	
        	}
        	Object[] ob = (Object[])Array.newInstance(aliasedClass.getComponentType(), 0);
        	value.set(typeDefObject,v.toArray(ob));
        	*/        	            		
        } else {        	
        	value.set(typeDefObject, obj);
        }
        return typeDefObject;
	}
	
	@Override
	public Object getAliasedObject(Object obj) throws SecurityException, NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
		Field value = obj.getClass().getField("value");
		Object objValue = value.get(obj);
		return objValue;	
	}
	
	@Override	
	public String toString() {
		return "SimpleTypeDef: " + super.toString();
	}
	
	
}
