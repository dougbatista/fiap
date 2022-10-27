import matplotlib.pyplot as plt
from sklearn.metrics import confusion_matrix, roc_curve, classification_report


def plot_lost_history(history):
    plt.plot(history.epoch, history.history["accuracy"], 'b', label='Accuracy')
    plt.plot(history.epoch, history.history["val_accuracy"], 'y', label='Validation Accuracy')
    plt.plot(history.epoch, history.history["loss"], 'g', label='Training loss')
    plt.plot(history.epoch, history.history["val_loss"], 'r', label='Validation loss')
    plt.title('Training loss')
    plt.xlabel('Epochs')
    plt.ylabel('Loss / Accuracy')
    plt.legend()
    plt.show()

def plot_lost_only(history):
    plt.plot(history.epoch, history.history["loss"], 'g', label='Training loss')
    plt.plot(history.epoch, history.history["val_loss"], 'r', label='Validation loss')
    plt.title('Training loss')
    plt.xlabel('Epochs')
    plt.ylabel('Loss')
    plt.legend()
    plt.show()
    
def plot_roc_curve(y_test, predicted):
    fp, tp, _ = roc_curve(y_test, predicted)

    plt.plot(fp, tp, color = 'red', label='ROC')
    plt.plot([0,1], [0, 1], color='green', linestyle='--')
    plt.xlabel('Falso Positivo')

    plt.ylabel('Verdadeiro positivo')
    plt.title('ROC CURVE')
    plt.legend()
    plt.show()

def report(y_test, predicted):
    print(classification_report(y_test, predicted))

def _confusion_matrix(y_test, predicted):
    print(confusion_matrix(y_test, predicted))

def append_to_binary_results(y_hat, threshold):
    results = []
    for i in range(len(y_hat)):
        if y_hat[i] >= threshold:
            results.append(1)
        else:
            results.append(0)
    
    return results
