package it.imolinfo.jbi4corba.webservice.generator.typedef;

import it.imolinfo.jbi4corba.Logger;
import it.imolinfo.jbi4corba.LoggerFactory;
import it.imolinfo.jbi4corba.exception.ClassGenerationException;
import it.imolinfo.jbi4corba.webservice.generator.Util;
import it.imolinfo.jbi4corba.webservice.generator.bcm.ByteCodeManipulationUtil;

import java.io.File;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlTransient;

import org.objectweb.asm.AnnotationVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.FieldVisitor;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;

/**
 * Complex Array TypeDef class generator.
 * 
 * The class generated is:
 *
 * <pre>
 * {@code
 *
 *
 * @XmlAccessorType(XmlAccessType.FIELD)
 * public class MyComplexTypeSeq{
 *   @XmlTransient
 *   final static private MyComplexTypeHelper helperClass = MyComplexTypeSeqHelper.class;
 *   @XmlElement(required=true, nillable=false)
 *   MyComplexType[] value;
 *  }
 * }
 * </pre>
 * 
 * @author <a href="mailto:mpiraccini@imolinfo.it">Marco Piraccini</a>
 */
public class ComplexArrayTypeDef extends SimpleTypeDef {
	
	/**
	 * Class logger
	 */
	private static final Logger LOG = LoggerFactory
			.getLogger(ComplexArrayTypeDef.class);	
	
	@SuppressWarnings("unchecked")
	protected ComplexArrayTypeDef(String className, String helperClassName, Class aliasedClass, String id) {
		super(className, helperClassName, aliasedClass, id);		
	}
	
	@SuppressWarnings("unchecked")
	public void createTypeDefClass(String classesDir) throws ClassGenerationException {
		// I have to calculate this values (i cannot load the class...doesn't exist yet)
		String classInternalName = this.getClassName().replace('.','/');
		String classDescriptor = "L" + classInternalName + ";";
		String aliasedClassDescriptor =  Type.getDescriptor(aliasedClass);
				
		// helper class
		Class helperClass = this.getHelperClass(classesDir);  		
		String helperDescriptor = Type.getDescriptor(helperClass);	
				
		// Class, Object and jaxb annotations
		String javaLangClassDescriptor = Type.getDescriptor(Class.class);
		String javaLangObjectInternalName = Type.getInternalName(java.lang.Object.class);		
		String xmlAccessorTypeDescriptor = Type.getDescriptor(XmlAccessorType.class);
		String xmlElementDescriptor = Type.getDescriptor(XmlElement.class);
		String xmlAccessTypeDescriptor = Type.getDescriptor(XmlAccessType.class);
		String xmlTransientDescriptor = Type.getDescriptor(XmlTransient.class);
								
		ClassWriter cw = new ClassWriter(true);
		
		// The class
		cw.visit(Opcodes.V1_5, ACC_PUBLIC + ACC_SUPER, classInternalName ,null, javaLangObjectInternalName, null);
		
		// The class XmlType Annotation		
		// The class XmlAccessorType Annotation:	@XmlAccessorType(XmlAccessType.FIELD)
		AnnotationVisitor av = cw.visitAnnotation(xmlAccessorTypeDescriptor, true);		  
        //the Annotations was changed for generate WSDL with Union Type
        av.visitEnum("value", xmlAccessTypeDescriptor, "FIELD"); 
		av.visitEnd();	
		
		/* 
		 * @XmlTransient
		 * final static private Class helperClass = null; 		// cannot be inited (null) 	
		 */	
		/*
		FieldVisitor fvHelperClass =  cw.visitField(ACC_PUBLIC + ACC_STATIC + ACC_FINAL
				, "helperClass", javaLangClassDescriptor, null, null);
		AnnotationVisitor avHelper = fvHelperClass.visitAnnotation(xmlTransientDescriptor, true);
		avHelper.visitEnd();			
		fvHelperClass.visitEnd();
		*/		
								
		/* The field with the XmlElement
		 * For example: 	
		 * @XmlElement(required=true, nillable=false)
		 * public int value;
		 */			
		FieldVisitor fv = cw.visitField(ACC_PUBLIC, "value", aliasedClassDescriptor, null, null);
		AnnotationVisitor avValue = fv.visitAnnotation(xmlElementDescriptor, true);
		avValue.visit("required", true);
		avValue.visit("nillable", false);
		avValue.visitEnd();		
		fv.visitEnd();
					
		
		// Class init. Inits the statoc "helperClass" field.
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
	
	public String toString() {
		return "ComplexArrayTypeDef: " + super.toString();
	}

	
//	// The same of SimpeTypeDef...probably it make more sense to have only TWO typedef sublclasses 
//	// (inheritance vs wrapper).	
//	@Override
//	public Object getTypeDefObject(ClassLoader classLoader, Object obj) throws ClassNotFoundException, InstantiationException, IllegalAccessException, SecurityException, NoSuchFieldException {
//		Class classDefinition = classLoader.loadClass(getClassName());
//        Object typeDefObject = classDefinition.newInstance();        
//        Field value = typeDefObject.getClass().getField("value");
//        LOG.info("Trying to set: " + obj + " of object: " + obj + " on object:" + typeDefObject);    	
//    	// This seems the only way to cast a Object[] to a String[]!!!!!
//        if (obj.getClass().isArray()) {
//        	
//        	if (LOG.isDebugEnabled()) {
//        		LOG.debug("Input: " + ArrayUtils.toString(obj));
//        	}
//         	Class aliasedClassCorrectCL = Class.forName(this.getAliasedClassName(), false, classLoader);        	        	        	        
//        	Object ob = TypeDefUtil.fillArray(obj, null, aliasedClassCorrectCL.getComponentType());
//        	if (LOG.isDebugEnabled()) {
//        		LOG.debug("Returned: " + ArrayUtils.toString(ob));
//        	}
//        	value.set(typeDefObject,ob);
//        	
//        	/*
//        	Vector v = new Vector();
//        	Object[] myarr = (Object[]) obj;  
//        	for (int i = 0; i < myarr.length; i++) {      
//        		v.add(myarr[i]);        		        	
//        	}
//        	Object[] ob = (Object[])Array.newInstance(aliasedClassCorrectCL.getComponentType(), 0);
//        	value.set(typeDefObject,v.toArray(ob));
//        	 */  
//          	
//        }  else {        	
//        	value.set(typeDefObject, obj);
//        }
//        return typeDefObject;
//	}
//	
//	@Override
//	public Object getAliasedObject(Object obj) throws SecurityException, NoSuchFieldException, IllegalArgumentException, IllegalAccessException {
//		Field value = obj.getClass().getField("value");
//		Object objValue = value.get(obj);
//		return objValue;	
//	}

	

}
