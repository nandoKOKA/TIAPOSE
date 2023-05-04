import cv2

# Load the pre-trained classifier for object detection
classifier = cv2.CascadeClassifier('haarcascade_frontalface_default.xml')

# Start the video capture using the default camera
capture = cv2.VideoCapture(0)

# Loop over the frames captured from the camera
while True:
    # Read the current frame
    ret, frame = capture.read()

    # Convert the frame to grayscale
    gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)

    # Detect objects using the classifier
    objects = classifier.detectMultiScale(gray, scaleFactor=1.3, minNeighbors=5)

    # Draw bounding boxes around the detected objects
    for (x, y, w, h) in objects:
        cv2.rectangle(frame, (x, y), (x + w, y + h), (0, 255, 0), 2)

    # Display the frame with the bounding boxes
    cv2.imshow('Object Detection', frame)

    # Wait for the user to press the 'q' key to quit
    if cv2.waitKey(1) & 0xFF == ord('q'):
        break

# Release the video capture and close the window
capture.release()
cv2.destroyAllWindows()