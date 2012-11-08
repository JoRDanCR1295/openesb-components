Following changes are made to build script to build and test in Test Driver framework.

1. ApplicantEvaluator\nbproject\build-impl.xml
Commented the build task on depending-on project under target "-deps-module-jar".

2. CreditApplicationProcessorApplication\build.xml
Added below task to target pre-init"
          <ant target="dist" inheritall="false" dir="../PerformanceEvaluator"/>
          <ant target="dist" inheritall="false" dir="../FICOSimulator"/>
          <ant target="wsimport-client-generate" inheritall="false" dir="../ApplicantEvaluator"/>
