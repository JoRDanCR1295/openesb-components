package it.imolinfo.jbi4corba.test.webservice.generator.complexintType;


/**
* it/imolinfo/jbi4corba/test/webservice/generator/complexintType/_Echo2Stub.java .
* Generated by the IDL-to-Java compiler (portable), version "3.2"
* from Test-IntType-Complex-multInterface.idl
* luned� 19 gennaio 2009 9.37.07 CET
*/

public class _Echo2Stub extends org.omg.CORBA.portable.ObjectImpl implements it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo2
{

  public it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo echo (it.imolinfo.jbi4corba.test.webservice.generator.complexintType.EchoStruct msg)
  {
            org.omg.CORBA.portable.InputStream $in = null;
            try {
                org.omg.CORBA.portable.OutputStream $out = _request ("echo", true);
                it.imolinfo.jbi4corba.test.webservice.generator.complexintType.EchoStructHelper.write ($out, msg);
                $in = _invoke ($out);
                it.imolinfo.jbi4corba.test.webservice.generator.complexintType.Echo $result = it.imolinfo.jbi4corba.test.webservice.generator.complexintType.EchoHelper.read ($in);
                return $result;
            } catch (org.omg.CORBA.portable.ApplicationException $ex) {
                $in = $ex.getInputStream ();
                String _id = $ex.getId ();
                throw new org.omg.CORBA.MARSHAL (_id);
            } catch (org.omg.CORBA.portable.RemarshalException $rm) {
                return echo (msg        );
            } finally {
                _releaseReply ($in);
            }
  } // echo

  // Type-specific CORBA::Object operations
  private static String[] __ids = {
    "IDL:it/imolinfo/jbi4corba/test/webservice/generator/complexintType/Echo2:1.0"};

  public String[] _ids ()
  {
    return (String[])__ids.clone ();
  }

  private void readObject (java.io.ObjectInputStream s) throws java.io.IOException
  {
     String str = s.readUTF ();
     String[] args = null;
     java.util.Properties props = null;
     org.omg.CORBA.Object obj = org.omg.CORBA.ORB.init (args, props).string_to_object (str);
     org.omg.CORBA.portable.Delegate delegate = ((org.omg.CORBA.portable.ObjectImpl) obj)._get_delegate ();
     _set_delegate (delegate);
  }

  private void writeObject (java.io.ObjectOutputStream s) throws java.io.IOException
  {
     String[] args = null;
     java.util.Properties props = null;
     String str = org.omg.CORBA.ORB.init (args, props).object_to_string (this);
     s.writeUTF (str);
  }
} // class _Echo2Stub
